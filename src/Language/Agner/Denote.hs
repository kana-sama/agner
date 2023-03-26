module Language.Agner.Denote where

import Language.Agner.Prelude

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef, modifyIORef')

import Control.Monad.IO.Cont (PromptTag, newPromptTag, prompt, control0)
import Control.Concurrent (threadDelay)

import Language.Agner.Syntax (FunId)
import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Value (Value, PID)
import Language.Agner.Value qualified as Value
import Language.Agner.Prettier qualified as Prettier
import Language.Agner.BiF qualified as BiF

data Ex
  = UnboundVariable Syntax.Var Env
  | UndefinedFunction Syntax.FunId
  | UnknownBiF Syntax.FunId
  | NoMatch Syntax.Pat Value Env
  | BinOp_BadArgs Syntax.BinOp
  | NoEntryPoint
  | NoFunctionClauseMatching Syntax.FunId [Value]
  | BadFunction Value
  | BadArity Syntax.FunId Int
  | Custom Value
  deriving stock (Show)
  deriving anyclass (Exception)

data Process = MkProcess
  { pid :: PID
  , body :: IO ()
  , mailbox :: [Value]
  } deriving stock (Generic)

data SchedulerState = MkSchedulerState
  { queue :: [Process]
  , current :: ~Process
  , fuel :: Int
  , gen :: Integer
  } deriving stock (Generic)

type Env = Map Syntax.Var Value
type FunEnv = Map Syntax.FunId ([Value] -> IO Value)

data YLog = MkYLog{id :: Int, pid :: Integer, name :: String, fuel :: Int}

type WithScheduler =
  ( ?state :: IORef SchedulerState
  , ?promptTag :: PromptTag ()
  , ?yLogs :: Maybe (IORef (Int, [YLog]))
  )

scheduler :: (State SchedulerState a) -> (WithScheduler => IO a)
scheduler action = atomicModifyIORef' ?state \s ->
  let (s', a) = runState action s in (a, s')

withScheduler :: Maybe (IORef (Int, [YLog])) -> (WithScheduler => IO ()) -> IO ()
withScheduler yLogs action = do
  state <- newIORef initialSchedulerState; let ?state = state
  promptTag <- newPromptTag; let ?promptTag = promptTag
  let ?yLogs = yLogs
  do spawn action; switch
  where
    initialSchedulerState = MkSchedulerState
      { queue = []
      , fuel = 0
      , current = undefined
      , gen = 0
      }

_FUEL = 10

push :: WithScheduler => Process -> IO ()
push p = scheduler do #queue <>= [p]

pop :: WithScheduler => IO (Maybe Process)
pop = do
  queue <- scheduler do use #queue
  case queue of
    [] -> pure Nothing
    p:ps -> do
      scheduler do #queue .= ps
      pure (Just p)

freshPID :: WithScheduler => IO Value.PID
freshPID = do
  pid <- scheduler do #gen <<+= 1
  pure (Value.MkPID pid)

self :: WithScheduler => IO Value.PID
self = scheduler do use (#current . #pid)

spawn :: WithScheduler => IO () -> IO Value.PID
spawn action = do
  pid <- freshPID
  push MkProcess{pid, body = do action; switch, mailbox = []}
  pure pid

ylog :: WithScheduler => FunId -> IO ()
ylog funid =
  case ?yLogs of
    Nothing -> pure ()
    Just ref -> do
      pid <- (.unPID) <$> self
      let name = Prettier.string Prettier.funIdA funid
      fuel <- scheduler do use #fuel
      modifyIORef' ref \(id, logs) -> (id+1, MkYLog{id, pid, name, fuel}:logs)

yield :: WithScheduler => FunId -> IO ()
yield funid = do
  ylog funid
  fuel <- scheduler do #fuel <-= 1
  when (fuel <= 0) do
    control0 ?promptTag \next -> do
      current <- scheduler do use #current
      push current{body = next (pure ())}
      switch

yieldForce :: WithScheduler => FunId -> IO ()
yieldForce funid = do
  scheduler do #fuel .= 1
  yield funid

switch :: WithScheduler => IO ()
switch = do
  pop >>= \case
    Nothing -> pure ()
    Just process -> do
      scheduler do
        #fuel .= _FUEL
        #current .= process
      prompt ?promptTag process.body

send :: WithScheduler => PID -> Value -> IO ()
send pid value = scheduler do
  #current . filtered (\p -> p.pid == pid) . #mailbox <>= [value]
  #queue . each . filtered (\p -> p.pid == pid) . #mailbox <>= [value]

binOp :: Syntax.BinOp -> (Value -> Value -> Value)
binOp = \case
  (Syntax.:+) -> \a b ->
    case (a, b) of
      (Value.Integer a, Value.Integer b) -> Value.Integer (a + b)
      _ -> throw (BinOp_BadArgs (Syntax.:+))
  (Syntax.:-) -> \a b ->
    case (a, b) of
      (Value.Integer a, Value.Integer b) -> Value.Integer (a - b)
      _ -> throw (BinOp_BadArgs (Syntax.:-))
  (Syntax.:++) -> \a b ->
    case a of
      Value.List a -> foldr Value.Cons b a
      _ -> throw (BinOp_BadArgs (Syntax.:++))
      

funDecl :: WithScheduler => FunEnv -> Syntax.FunDecl -> ([Value] -> IO Value)
funDecl funs decl args = do
  yield decl.funid
  goClauses decl.clauses args
  where
    goClauses [] =
      \values -> throw (NoFunctionClauseMatching decl.funid values)
    goClauses (c:cs) =
      \values ->
        case matchClause c.pats values Map.empty of
          Nothing -> goClauses cs values
          Just env -> do
            (v, _) <- (let ?funs = funs in exprs c.body) env
            pure v

    matchClause :: [Syntax.Pat] -> [Value] -> Env -> Maybe Env
    matchClause [] [] env = Just env
    matchClause (p:ps) (v:vs) env =
      case match p v env of
        Just env -> matchClause ps vs env
        Nothing -> Nothing
    matchClause _ _ _ = error "Denote.tryClause: impossible!!"

expr :: (WithScheduler, ?funs :: FunEnv) => Syntax.Expr -> (Env -> IO (Value, Env))
expr = \case
  Syntax.Integer i -> runStateT do
    pure (Value.Integer i)
  Syntax.Atom a -> runStateT do
    pure (Value.Atom a)
  Syntax.Fun f -> runStateT do
    pure (Value.Fun f)
  Syntax.Tuple es -> runStateT do
    vs <- for es \e -> StateT (expr e)
    pure (Value.Tuple vs)
  Syntax.Nil -> runStateT do
    pure Value.Nil
  Syntax.Cons a b -> runStateT do
    a <- StateT (expr a)
    b <- StateT (expr b)
    pure (Value.Cons a b)
  Syntax.BinOp a op b -> runStateT do
    a <- StateT (expr a)
    b <- StateT (expr b)
    pure ((binOp op) a b)
  Syntax.Var var -> runStateT do
    env <- get
    case env Map.!? var of
      Just val -> pure val
      Nothing -> throw (UnboundVariable var env)
  Syntax.Match p e -> runStateT do
    v <- StateT (expr e)
    env <- get
    case (match p v) env of
      Nothing -> throw (NoMatch p v env)
      Just env -> put env *> pure v
  Syntax.Apply _ funid args ->
    apply funid args
  Syntax.DynApply e args -> runStateT do
    StateT (expr e) >>= \case
      Value.Fun funid -> StateT (apply funid args)
      value -> throw (BadFunction value)
  Syntax.Send a b ->
    apply "erlang:send/2" [a, b]
  Syntax.Receive cases -> receive cases

receive :: (WithScheduler, ?funs :: FunEnv) => [(Syntax.Pat, Syntax.Exprs)] -> (Env -> IO (Value, Env))
receive cases env = do
  msgs <- scheduler do use (#current . #mailbox)
  go msgs [] cases env
  where
    go (msg:msgs) not_matched ((p, es):cases) env =
      case match p msg env of
        Nothing -> go (msg:msgs) not_matched cases env
        Just env -> do
          scheduler do #current . #mailbox .= reverse not_matched ++ msgs
          exprs es env
    go (msg:msgs) not_matched [] env =
      go msgs (msg:not_matched) cases env
    go [] _ _ env = do
      yieldForce "receive/0"
      receive cases env

  

apply :: (WithScheduler, ?funs :: FunEnv) => Syntax.FunId -> [Syntax.Expr] -> (Env -> IO (Value, Env))
apply funid args = runStateT do
  when (funid.arity /= length args) do
    throw (BadArity funid (length args))
  let !fun = resolveFunction funid
  vals <- traverse (StateT . expr) args
  lift do fun vals

match :: Syntax.Pat -> Value -> (Env -> Maybe Env)
match Syntax.PatWildcard _ =
  \env -> Just env
match (Syntax.PatInteger i) value
  | Value.same (Value.Integer i) value = \env -> Just env
  | otherwise = \env -> Nothing
match (Syntax.PatAtom a) value
  | Value.same (Value.Atom a) value = \env -> Just env
  | otherwise = \env -> Nothing
match (Syntax.PatTuple ps) value
  | Value.Tuple vs <- value, length ps == length vs =
      \env -> foldlM (\env (p, v) -> match p v env) env (zip ps vs) 
  | otherwise = \env -> Nothing
match Syntax.PatNil value
  | Value.Nil <- value = \env -> Just env
  | otherwise = \env -> Nothing
match (Syntax.PatCons pa pb) value
  | Value.Cons a b <- value = match pa a >=> match pb b
  | otherwise = \env -> Nothing
match (Syntax.PatVar var) val =
  \env ->
    case env Map.!? var of
      Nothing -> Just (Map.insert var val env)
      Just val'
        | Value.same val val' -> Just env
        | otherwise -> Nothing

exprs :: (WithScheduler, ?funs :: FunEnv) => Syntax.Exprs -> (Env -> IO (Value, Env))
exprs [] = nonEmptyError "Denote.exprs"
exprs (e : es) = runStateT do
  v <- StateT (expr e)
  foldlM (\_ e -> StateT (expr e)) v es

bif :: (WithScheduler, ?funs :: FunEnv) => BiF.BiF -> ([Value] -> IO Value)
bif b args = BiF.runSpec alg (BiF.spec b args)
  where
    alg = \case
      BiF.RunIO io k -> k <$> io
      BiF.Spawn funid k -> k <$> spawn (void (resolveFunction funid []))
      BiF.Yield funid k -> k <$ yield funid
      BiF.Self k -> k <$> self
      BiF.Send pid value k -> k <$ send pid value
      BiF.Error e -> throw (Custom e)

resolveFunction :: (WithScheduler, ?funs :: FunEnv) => Syntax.FunId -> ([Value] -> IO Value)
resolveFunction funid | Just b <- BiF.parse funid = bif b
resolveFunction funid | Just f <- ?funs Map.!? funid = f
resolveFunction funid = throw (UndefinedFunction funid)

module_ :: WithScheduler => Syntax.Module -> IO ()
module_ mod = do
  let funs = Map.fromList [(d.funid, funDecl funs d) | d <- mod.decls]
  case funs Map.!? ("main" Syntax.:/ 0) of
    Nothing -> throw NoEntryPoint
    Just main -> do
      main []
      pure ()

denoteWithYLogs :: (WithScheduler => IO ()) -> IO [YLog]
denoteWithYLogs action = do
  yLogs <- newIORef (1, [])
  withScheduler (Just yLogs) action
  reverse . snd <$> readIORef yLogs

denote :: (WithScheduler => IO ()) -> IO ()
denote = withScheduler Nothing