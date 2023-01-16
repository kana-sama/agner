module Language.Agner.SM (Ex(..), Prog, Instr(..), compileModule, run) where

import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Control.Exception (Exception, throw)

import Control.Monad.State.Strict

import Control.Lens
import GHC.Generics (Generic)
import Data.Generics.Labels

import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Value (Value)
import Language.Agner.Value qualified as Value
import Language.Agner.Denote qualified as Denote
import Language.Agner.Syntax (BinOp)

data Ex
  = NotEnoughValuesForResult
  | EmptyStack
  | AlreadyHalted
  | PositionIsOutOfProg
  | UnboundVariable Syntax.Var
  | NoMatch Syntax.Pat Value
  deriving stock (Show)
  deriving anyclass (Exception)

data Instr
  = PUSH_I Integer
  | BINOP Syntax.BinOp
  | DUP
  | DROP
  | RET
  | ENTER String [Syntax.Var]
  | LEAVE String
  | LOAD Syntax.Var
  | MATCH_VAR Syntax.Var
  | MATCH_I Integer

type Prog = [Instr]

compileExpr :: Syntax.Expr -> Prog
compileExpr = \case
  Syntax.Integer i ->
    [PUSH_I i]
  Syntax.BinOp a op b ->
    compileExpr a ++ compileExpr b ++ [BINOP op]
  Syntax.Var var ->
    [LOAD var]
  Syntax.Match p e ->
    compileExpr e ++ [DUP] ++ compilePat p

compilePat :: Syntax.Pat -> Prog
compilePat = \case
  Syntax.PatWildcard -> [DROP]
  Syntax.PatInteger i -> [MATCH_I i]
  Syntax.PatVar var -> [MATCH_VAR var]

compileExprs :: Syntax.Exprs -> Prog
compileExprs exprs = List.intercalate [DROP] [compileExpr e | e <- NonEmpty.toList exprs]

compileModule :: Syntax.Module -> Prog
compileModule mod =
  concat
    [ [ENTER "main" (Set.toList (Syntax.moduleVars mod))]
    , compileExprs mod
    , [LEAVE "main"]
    , [RET]
    ]

data Cfg = MkCfg
  { pos :: Int
  , prog :: Prog
  , stack :: [Value]
  , mem :: Map String Value
  , halted :: Bool
  }
  deriving stock (Generic)

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

instr :: Instr -> (Cfg -> Cfg)
instr (PUSH_I i) = execState do
  push (Value.Integer i)
  continue
instr (BINOP op) = execState do
  b <- pop
  a <- pop
  push ((Denote.binOp op) a b)
  continue
instr DUP = execState do
  x <- pop
  push x
  push x
  continue
instr DROP = execState do
  void pop
  continue
instr RET = execState do
  halt
instr (ENTER name vars) = execState do
  continue
instr (LEAVE name) = execState do
  continue
instr (LOAD var) = execState do
  mem <- use #mem
  case mem Map.!? var of
    Nothing -> throw (UnboundVariable var)
    Just val -> do
      push val
      continue
instr (MATCH_I i) = execState do
  v <- pop
  unless (Value.same v (Value.Integer i)) do
    throw (NoMatch (Syntax.PatInteger i) v)
  continue
instr (MATCH_VAR var) = execState do
  mem <- use #mem
  val <- pop
  case mem Map.!? var of
    Nothing -> do
      #mem %= Map.insert var val
      continue
    Just val'
      | Value.same val val' -> continue
      | otherwise -> throw (NoMatch (Syntax.PatVar var) val)

step :: Cfg -> Cfg
step cfg
  | cfg.halted = throw AlreadyHalted
  | otherwise = instr (currentInstr cfg) cfg
  where
    currentInstr (cfg :: Cfg)
      | cfg.pos >= length cfg.prog = throw PositionIsOutOfProg
      | otherwise = cfg.prog !! cfg.pos

buildCfg :: Prog -> Int -> Cfg
buildCfg prog pos =
  MkCfg
    { prog
    , pos
    , stack = []
    , mem = Map.empty
    , halted = False
    }

run :: Prog -> Value
run prog =
  case (go (buildCfg prog 0)).stack of
    result:_ -> result
    _ -> throw NotEnoughValuesForResult
  where
    go cfg
      | cfg.halted = cfg
      | otherwise = go (step cfg)