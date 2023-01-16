module Data.X64 where

import Data.List qualified as List
import Control.Monad.Writer (MonadWriter, tell)

type Label = String

data Op = MOVQ | SUBQ | ADDQ | PUSHQ | POPQ | RETQ
  deriving stock (Show)

data Reg = RAX | RBX | RCX | RDX | RSI | RDI | RSP | RBP
  deriving stock (Show, Enum, Bounded)

data Operand
  = Reg Reg -- RAX
  | MemReg Int Reg -- -8(%RSP)
  | Imm Integer -- $8
  | ImmL String

data Instr
  = Op Op [Operand]
  | Label Label
  | Set String Int
  | Meta String

type Prog = [Instr]

rax, rbx, rcx, rdx, rsi, rdi, rsp, rbp :: Operand
[rax, rbx, rcx, rdx, rsi, rdi, rsp, rbp] = [Reg r | r <- [RAX ..]]

movq, subq, addq :: MonadWriter Prog m => Operand -> Operand -> m ()
movq a b = tell [Op MOVQ [a, b]]
subq a b = tell [Op SUBQ [a, b]]
addq a b = tell [Op ADDQ [a, b]]

pushq, popq :: MonadWriter Prog m => Operand -> m ()
pushq a = tell [Op PUSHQ [a]]
popq a = tell [Op POPQ [a]]

retq :: MonadWriter Prog m => m ()
retq = tell [Op RETQ []]

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