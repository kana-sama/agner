module Language.Agner.X64 where

import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.Foldable (traverse_, for_)
import Data.Traversable (for)
import Control.Lens
import Data.Generics.Labels
import GHC.Generics (Generic)
import Data.List qualified as List

import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Value (Value)
import Language.Agner.Value qualified as Value
import Language.Agner.SM qualified as SM

data Op
  = MOVQ
  | SUBQ
  | PUSHQ
  | POPQ
  | RETQ
  deriving stock (Show)

data Reg
  = RAX
  | RBX
  | RCX
  | RDX
  | RSI
  | RDI
  | RSP
  | RBP
  deriving stock (Show, Enum, Bounded)

[rax, rbx, rcx, rdx, rsi, rdi, rsp, rbp] = [Reg r | r <- [RAX ..]]

data Operand
  = Reg Reg -- RAX
  | MemReg Int Reg -- -8(%RSP)
  | Imm Integer -- $8
  | ImmL String

data Instr
  = Op Op [Operand]
  | Label String
  | Set String Int
  | Meta String

type Prog = [Instr]

#define WORD_SIZE 8

data CompileState = MkCompileState
  { maxAllocated :: Int
  , currentReg :: Int
  }
  deriving stock (Generic)

pool, poolRegs :: [Operand]
poolRegs = [Reg r | r <- [RBX]]
pool = poolRegs ++ [MemReg (-WORD_SIZE * i) RSP | i <- [1 ..]]

_alloc :: M Operand
_alloc = do
  currentReg <- #currentReg <<+= 1
  #maxAllocated %= max (currentReg + 1)
  pure (pool !! currentReg)

_pop :: M Operand
_pop = do
  currentReg <- #currentReg <-= 1
  pure (pool !! currentReg)

movq, subq :: Operand -> Operand -> M ()
movq a b = tell [Op MOVQ [a, b]]
subq a b = tell [Op SUBQ [a, b]]

pushq, popq :: Operand -> M ()
pushq a = tell [Op PUSHQ [a]]
popq a = tell [Op POPQ [a]]

retq :: M ()
retq = tell [Op RETQ []]

type M = StateT CompileState (Writer Prog)

execM :: M a -> Prog
execM = execWriter . flip runStateT emptyState
  where
    emptyState = MkCompileState{maxAllocated = 0, currentReg = 0}

compileInstr :: SM.Instr -> M ()
compileInstr = \case
  SM.PUSHI x -> do
    d <- _alloc
    movq (Imm x) d

compileProg :: SM.Prog -> M ()
compileProg prog = do
  #maxAllocated .= 0
  #currentReg .= 0

  pushq rbp
  movq rsp rbp
  subq (ImmL localsVar) rsp

  traverse_ compileInstr prog
  s <- _pop
  movq s rax

  locals <- uses #maxAllocated (subtract (length poolRegs))
  tell [Set localsVar (locals * WORD_SIZE)]

  movq rbp rsp
  popq rbp

  retq
  where
    localsVar = "_locals"

prettyOperand :: Operand -> String
prettyOperand = \case
  Reg reg -> "%" ++ show reg
  MemReg offset reg -> show offset ++ "(%" ++ show reg ++ ")"
  Imm i -> "$" ++ show i
  ImmL l -> "$" ++ l

prettyInstr :: Instr -> String
prettyInstr = \case
  Op op ops -> "    " ++ show op ++ " " ++ List.intercalate ", " [prettyOperand o | o <- ops]
  Label l -> l ++ ":"
  Set l i -> ".set " ++ l ++ ", " ++ show i
  Meta m -> m

prettyProg :: Prog -> String
prettyProg = unlines . map prettyInstr

compile :: SM.Prog -> Prog
compile prog = execM do
  tell [Meta ".text"]
  tell [Meta ".globl _main"]

  tell [Label "_main"]
  compileProg prog