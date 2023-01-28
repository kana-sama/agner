module Language.Agner.SM (Ex(..), Prog, Instr(..), compileModule, debug, run) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Aeson (ToJSON)

import Control.Exception (Exception (..), try, throw, SomeException, evaluate)
import System.IO.Unsafe

import Control.Monad.State.Strict

import Control.Lens
import GHC.Generics (Generic)
import Data.Generics.Labels

import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Value (Value)
import Language.Agner.Value qualified as Value
import Language.Agner.Denote qualified as Denote
import Language.Agner.Syntax (BinOp)

import Debug.Trace

data Ex
  = NotEnoughValuesForResult
  | EmptyStack
  | AlreadyHalted
  | PositionIsOutOfProg
  | NoMatch Syntax.Pat Value
  | UnboundVariable Syntax.Var
  | UndefinedFuncton Syntax.FunId
  | DenoteEx Denote.Ex
  | NoFunctionClauseMatching Syntax.FunId [Value]
  | NoEntryPoint
  deriving stock (Show)

instance Exception Ex where
  fromException e
    | Just de <- fromException @Denote.Ex e = Just (DenoteEx de)
    | otherwise = Nothing

data OnMatchFail = Throw | NextClause
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data Instr
  = PUSH_I Integer
  | PUSH_ATOM Syntax.Atom
  | BINOP Syntax.BinOp
  | DROP
  | DUP
  | CALL Syntax.FunId
  
  | FUNCTION Syntax.FunId
  | CLAUSE Syntax.FunId
  | FAIL_CLAUSE Syntax.FunId
  | LEAVE Syntax.FunId
  | RET

  | LOAD Syntax.Var
  | MATCH_I Integer OnMatchFail
  | MATCH_VAR Syntax.Var OnMatchFail
  | MATCH_ATOM Syntax.Atom OnMatchFail

  deriving stock (Generic)
  deriving anyclass (ToJSON)

type Prog = [Instr]

compileExpr :: Syntax.Expr -> Prog
compileExpr = \case
  Syntax.Integer i ->
    [PUSH_I i]
  Syntax.Atom a ->
    [PUSH_ATOM a]
  Syntax.BinOp a op b ->
    compileExpr a ++ compileExpr b ++ [BINOP op]
  Syntax.Var v ->
    [LOAD v]
  Syntax.Match p e ->
    compileExpr e ++ [DUP] ++ compilePat Throw p
  Syntax.Apply f args -> do
    foldMap compileExpr args ++ [CALL (f Syntax.:/ length args)]

compilePat :: OnMatchFail -> Syntax.Pat -> Prog
compilePat onMatchFail = \case
  Syntax.PatVar var -> [MATCH_VAR var onMatchFail]
  Syntax.PatInteger i -> [MATCH_I i onMatchFail]
  Syntax.PatWildcard -> [DROP]
  Syntax.PatAtom a -> [MATCH_ATOM a onMatchFail]

compileExprs :: Syntax.Exprs -> Prog
compileExprs exprs = List.intercalate [DROP] [compileExpr e | e <- NonEmpty.toList exprs]


compileFunDecl :: Syntax.FunDecl -> Prog
compileFunDecl funDecl =
  concat
    [ [FUNCTION funDecl.funid]
    , foldMap compileFunClause funDecl.clauses
    , [FAIL_CLAUSE funDecl.funid]
    ]

-- args are in original order on stack
compileFunClause :: Syntax.FunClause -> Prog
compileFunClause clause =
  concat
    [ [CLAUSE clause.funid]
    , foldMap (compilePat NextClause) clause.pats
    , compileExprs clause.body
    , [LEAVE clause.funid]
    , [RET]
    ]

compileModule :: Syntax.Module -> Prog
compileModule mod =
  foldMap compileFunDecl mod.decls

data Frame = MkFrame
  { stack :: [Value]
  , mem :: Map Syntax.Var Value
  , ret :: Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data Cfg = MkCfg
  { pos :: Int
  , prog :: Prog
  , stack :: [Value]
  , mem :: Map Syntax.Var Value
  , frames :: [Frame]
  , halted :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

type M = State Cfg

push :: Value -> M ()
push v = #stack %= (v:)

pop :: M Value
pop = zoom #stack do
  state \case
    [] -> throw EmptyStack
    v:stack -> (v, stack)

continue :: M ()
continue = #pos += 1

halt :: M ()
halt = #halted .= True

currentInstr :: Cfg -> Instr
currentInstr cfg
  | cfg.pos >= length cfg.prog = throw PositionIsOutOfProg
  | otherwise = cfg.prog !! cfg.pos

skipForNextClause :: M ()
skipForNextClause = do
  instr <- gets currentInstr
  case instr of
    CLAUSE _ -> pure ()
    FAIL_CLAUSE _ -> pure ()
    _ -> continue *> skipForNextClause

type FunEnv = Map Syntax.FunId Int

leave :: M ()
leave = do
  frames <- use #frames
  case frames of
    [] -> pure ()
    frame:frames -> do
      #frames .= frames
      #stack .= frame.stack
      #mem .= frame.mem

instr :: (?funs :: FunEnv) => Instr -> (Cfg -> Cfg)
instr (PUSH_I i) = execState do
  push (Value.Integer i)
  continue
instr (PUSH_ATOM a) = execState do
  push (Value.Atom a)
  continue
instr (BINOP op) = execState do
  b <- pop
  a <- pop
  push ((Denote.binOp op) a b)
  continue
instr DROP = execState do
  void pop
  continue
instr DUP = execState do
  a <- pop
  push a
  push a
  continue
instr (CALL f) = execState do
  ret <- use #pos
  args <- replicateM f.arity pop
  stack <- #stack <<.= reverse args
  mem <- #mem <<.= Map.empty
  #frames %= (MkFrame{stack, mem, ret} :)
  case ?funs Map.!? f of
    Nothing -> throw (UndefinedFuncton f)
    Just pos -> #pos .= pos

instr (FUNCTION _) = execState do
  continue

instr (CLAUSE _) = execState do
  stack <- use #stack
  #frames %= (MkFrame{stack, mem = Map.empty, ret = 0} :)
  continue

instr (FAIL_CLAUSE f) = execState do
  vals <- reverse <$> replicateM f.arity pop
  throw (NoFunctionClauseMatching f vals)

instr (LEAVE _) = execState do
  result <- pop
  leave
  push result
  continue

instr RET = execState do
  use #frames >>= \case
    [] -> halt
    frame:_ -> do
      result <- pop
      leave
      push result
      #pos .= frame.ret
      continue

instr (LOAD var) = execState do
  mem <- use #mem
  case mem Map.!? var of
    Nothing -> throw (UnboundVariable var)
    Just val -> do
      push val
      continue
instr (MATCH_I i onMatchFail) = execState do
  value <- pop
  if Value.same (Value.Integer i) value
    then continue
    else case onMatchFail of
           Throw -> throw (NoMatch (Syntax.PatInteger i) value)
           NextClause -> leave *> skipForNextClause

instr (MATCH_VAR var onMatchFail) = execState do
  val <- pop
  mem <- use #mem
  case mem Map.!? var of
    Nothing -> do
      #mem %= Map.insert var val
      continue
    Just val'
      | Value.same val val' -> continue
      | otherwise ->
        case onMatchFail of
          Throw -> throw (NoMatch (Syntax.PatVar var) val)
          NextClause -> leave *> skipForNextClause

instr (MATCH_ATOM a onMatchFail) = execState do
  value <- pop
  if Value.same (Value.Atom a) value
    then continue
    else case onMatchFail of
          Throw -> throw (NoMatch (Syntax.PatAtom a) value)
          NextClause -> leave *> skipForNextClause

step :: (?funs :: FunEnv) => Cfg -> Cfg
step cfg
  | cfg.halted = throw AlreadyHalted
  | otherwise = instr (currentInstr cfg) cfg

buildCfg :: Prog -> Int -> Cfg
buildCfg prog pos =
  MkCfg
    { prog
    , pos
    , stack = []
    , mem = Map.empty
    , frames = []
    , halted = False
    }

data DebugOutput = MkDebugOutput
  { prog :: Prog
  , cfgs :: [Cfg]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

debug :: Int -> Prog -> DebugOutput
debug fuel prog =
  let ?funs = funs
   in MkDebugOutput{prog, cfgs = go fuel (buildCfg prog entryPoint)}
  where
    funs :: FunEnv
    funs = Map.fromList [(f, i) | (FUNCTION f, i) <- zip prog [0..]]

    entryPoint :: Int
    entryPoint =
      case funs Map.!? ("init" Syntax.:/ 0) of
        Nothing -> throw NoEntryPoint
        Just pos -> pos

    go 0 cfg = [cfg]
    go fuel cfg
      | cfg.halted = [cfg]
      | otherwise = unsafePerformIO do
        try @SomeException (evaluate (go (pred fuel) (step cfg))) >>= \case
          Left e -> do
            putStrLn (displayException e)
            pure [cfg]
          Right cfgs -> pure (cfg:cfgs)

run :: Prog -> Value
run prog =
  case (let ?funs = funs in go (buildCfg prog entryPoint)).stack of
    [result] -> result
    _ -> throw NotEnoughValuesForResult
  where
    funs :: FunEnv
    funs = Map.fromList [(f, i) | (FUNCTION f, i) <- zip prog [0..]]

    entryPoint :: Int
    entryPoint =
      case funs Map.!? ("init" Syntax.:/ 0) of
        Nothing -> throw NoEntryPoint
        Just pos -> pos

    go cfg
      | cfg.halted = cfg
      | otherwise = go (step cfg)