module Data.X64 where

import Data.List qualified as List
import Control.Monad.Writer (MonadWriter, tell)

type Label = String

data Op = MOVQ | SUBQ | ADDQ | PUSHQ | POPQ | RETQ | ANDQ | JMP | JZ | JE | JNZ | SYSCALL | CMPQ | CALLQ
  deriving stock (Show)

data Reg = RAX | RBX | RCX | RDX | RSI | RDI | RSP | RBP | RIP | R8 | R9
  deriving stock (Show, Enum, Bounded)

data Operand
  = Reg Reg -- RAX
  | MemReg Int Reg -- -8(%RSP)
  | MemRegL Label Reg -- qwe(%RBP)
  | Imm Integer -- $8
  | ImmL String
  | Lbl Label

instance Num Operand where
  fromInteger = Imm
  (+) = undefined; (-) = undefined; (*) = undefined; abs = undefined; signum = undefined

data Instr
  = Op Op [Operand]
  | Label Label
  | Set String Int
  | Meta String

type Prog = [Instr]

rax, rbx, rcx, rdx, rsi, rdi, rsp, rbp, rip, r8, r9 :: Operand
[rax, rbx, rcx, rdx, rsi, rdi, rsp, rbp, rip, r8, r9] = [Reg r | r <- [RAX ..]]

movq, subq, addq, andq, cmpq :: MonadWriter Prog m => Operand -> Operand -> m ()
movq a b = tell [Op MOVQ [a, b]]
subq a b = tell [Op SUBQ [a, b]]
addq a b = tell [Op ADDQ [a, b]]
andq a b = tell [Op ANDQ [a, b]]
cmpq a b = tell [Op CMPQ [a, b]]

pushq, popq :: MonadWriter Prog m => Operand -> m ()
pushq a = tell [Op PUSHQ [a]]
popq a = tell [Op POPQ [a]]

jmp, jz, je, callq :: MonadWriter Prog m => Label -> m ()
jmp l = tell [Op JMP [Lbl l]]
jz l = tell [Op JZ [Lbl l]]
je l = tell [Op JE [Lbl l]]
callq l = tell [Op CALLQ [Lbl l]]

retq, syscall :: MonadWriter Prog m => m ()
retq = tell [Op RETQ []]
syscall = tell [Op SYSCALL []]

_set :: MonadWriter Prog m => Label -> Int -> m ()
_set lbl value = tell [Set lbl value]

prettyOperand :: Operand -> String
prettyOperand = \case
  Reg reg -> "%" ++ show reg
  MemReg offset reg -> (if offset > 0 then "+" else "") ++ show offset ++ "(%" ++ show reg ++ ")"
  MemRegL lbl reg -> lbl ++ "(%" ++ show reg ++ ")"
  Imm i -> "$" ++ show i
  ImmL l -> "$" ++ l
  Lbl l -> l

prettyInstr :: Instr -> String
prettyInstr = \case
  Op op ops -> "    " ++ show op ++ " " ++ List.intercalate ", " [prettyOperand o | o <- ops]
  Label l -> l ++ ":"
  Set l i -> ".set " ++ l ++ ", " ++ show i
  Meta m -> m

prettyProg :: Prog -> String
prettyProg = unlines . map prettyInstr