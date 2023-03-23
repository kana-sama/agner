module Language.Agner.Denote where

import Language.Agner.Prelude

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, putMVar)
import Control.Monad.Reader
import Control.Monad.Trans.Cont

import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Value (Value)
import Language.Agner.Value qualified as Value

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

data SchedulerCtx = MkSchedulerCtx
  { queue :: [(Value.PID, M ())]
  , fuel :: Int
  , current :: Value.PID
  , gen :: Integer
  } deriving stock (Generic)

type M = ContT () (StateT SchedulerCtx IO)

runM :: M a -> IO a
runM action = do
  result <- newEmptyMVar
  let action' = liftIO . putMVar result =<< action
  evalStateT (runContT (do spawn action'; switch) pure) initial
  readMVar result
  where
    initial = MkSchedulerCtx
      { queue = []
      , fuel = 0
      , current = Value.MkPID (-1)
      , gen = 0
      }

_FUEL = 10

push :: Value.PID -> M () -> M ()
push pid action = do
  #queue <>= [(pid, action)]

freshPID :: M Value.PID
freshPID = do
  pid <- #gen <<+= 1
  pure (Value.MkPID pid)

spawn :: M () -> M Value.PID
spawn action = do
  pid <- freshPID
  push pid (do action; switch)
  pure pid

yield :: M ()
yield = do
  fuel <- #fuel <-= 1
  when (fuel == 0) do
    shiftT \next -> do
      pid <- use #current
      push pid (lift (next ()))
      switch

switch :: M ()
switch = do
  use #queue >>= \case
    [] -> pure ()
    (pid, task):tasks -> do
      #queue .= tasks
      #fuel .= _FUEL
      #current .= pid
      task

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

type Env = Map Syntax.Var Value
type FunEnv = Map Syntax.FunId ([Value] -> M Value)

funDecl :: FunEnv -> Syntax.FunDecl -> ([Value] -> M Value)
funDecl funs decl =
  \args -> do
    yield
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

expr :: (?funs :: FunEnv) => Syntax.Expr -> (Env -> M (Value, Env))
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

apply :: (?funs :: FunEnv) => Syntax.FunId -> [Syntax.Expr] -> (Env -> M (Value, Env))
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

exprs :: (?funs :: FunEnv) => Syntax.Exprs -> (Env -> M (Value, Env))
exprs [] = nonEmptyError "Denote.exprs"
exprs (e : es) = runStateT do
  v <- StateT (expr e)
  foldlM (\_ e -> StateT (expr e)) v es

module_ :: Syntax.Module -> IO Value
module_ mod = runM do
  let funs = Map.fromList [(d.funid, funDecl funs d) | d <- mod.decls] 
   in case funs Map.!? ("main" Syntax.:/ 0) of
        Nothing -> throw NoEntryPoint
        Just main -> main []

bifs :: Set Syntax.FunId
bifs = Set.fromList
  [ "agner:print/1"
  , "timer:sleep/1"
  , "error/1"
  , "spawn/1"
  , "self/0"
  ]

isBif :: Syntax.FunId -> Bool
isBif = (`Set.member` bifs)

bif :: (?funs :: FunEnv) => Syntax.FunId -> ([Value] -> M Value)
bif funid args = do
  yield
  body funid args
  where
    body = \case
      "self/0" -> \[] -> do
        pid <- use #current
        pure (Value.PID pid)
      "spawn/1" -> \[Value.Fun funid] -> do
        pid <- spawn (do resolveFunction funid []; pure ())
        pure (Value.PID pid)
      "agner:print/1" -> \[value] -> do
        liftIO do putStrLn (Value.encode value)
        pure (Value.Atom "ok")
      "error/1" -> \[value] -> do
        throw (Custom value)
      "timer:sleep/1" -> \[value] ->
        case value of
          Value.Integer duration -> do
            liftIO do threadDelay (fromInteger duration * 1000)
            pure (Value.Atom "ok")
          Value.Atom "infinity" -> do
            forever do liftIO do threadDelay (1000 * 1000)
          value -> do
            throw (NoFunctionClauseMatching "timer:sleep/1" [value])
      funid -> throw (UnknownBiF funid)

resolveFunction :: (?funs :: FunEnv) => Syntax.FunId -> ([Value] -> M Value)
resolveFunction funid | isBif funid = bif funid
resolveFunction funid | Just f <- ?funs Map.!? funid = f
resolveFunction funid = throw (UndefinedFunction funid)