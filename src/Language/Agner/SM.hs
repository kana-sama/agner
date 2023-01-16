module Language.Agner.SM (Ex(..), Prog, Instr(..), compileModule, run) where

import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty

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
  deriving stock (Show)
  deriving anyclass (Exception)

data Instr
  = PUSH_I Integer
  | BINOP Syntax.BinOp
  | DROP
  | RET
  | ENTER String
  | LEAVE String

type Prog = [Instr]
type Stack = [Value]

compileExpr :: Syntax.Expr -> Prog
compileExpr = \case
  Syntax.Integer i ->
    [PUSH_I i]
  Syntax.BinOp a op b ->
    compileExpr a ++ compileExpr b ++ [BINOP op]

compileExprs :: Syntax.Exprs -> Prog
compileExprs exprs = List.intercalate [DROP] [compileExpr e | e <- NonEmpty.toList exprs]

compileModule :: Syntax.Module -> Prog
compileModule mod = [ENTER "main"] ++ compileExprs mod ++ [LEAVE "main", RET]

data Cfg = MkCfg
  { pos :: Int
  , prog :: Prog
  , stack :: Stack
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
instr DROP = execState do
  void pop
  continue
instr RET = execState do
  halt
instr (ENTER name) = execState do
  continue
instr (LEAVE name) = execState do
  continue

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