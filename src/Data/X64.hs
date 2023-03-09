module Data.X64 where

import Data.List qualified as List
import Control.Monad.Writer (MonadWriter, tell)

type Label = String

data Op = LEAQ | MOVQ | MOVZBQ | SUBQ | ADDQ | PUSHQ | POPQ | RETQ | ANDQ | ORQ | XORQ | JMP | JZ | JE | JNE | JNZ | SYSCALL | CMPQ | CALLQ
  deriving stock (Show)

data Reg = RAX | RBX | RCX | RDX | RSI | RDI | RSP | RBP | RIP | R8 | R9 | R10 | R11 | R12 | R13
  deriving stock (Show, Enum, Bounded)

data Operand
  = Reg Reg -- RAX
  | MemReg Int Reg -- -8(%RSP)
  | MemRegL Label Reg -- qwe(%RBP)
  | Static Label -- lbl@GOTPCREL(%RIP)
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

rax, rbx, rcx, rdx, rsi, rdi, rsp, rbp, rip, r8, r9, r10, r11, r12, r13 :: Operand
[rax, rbx, rcx, rdx, rsi, rdi, rsp, rbp, rip, r8, r9, r10, r11, r12, r13] = [Reg r | r <- [RAX ..]]

leaq, movq, movzbq, subq, addq, orq, xorq, andq, cmpq :: MonadWriter Prog m => Operand -> Operand -> m ()
leaq a b = tell [Op LEAQ [a, b]]
movq a b = tell [Op MOVQ [a, b]]
movzbq a b = tell [Op MOVZBQ  [a, b]]
subq a b = tell [Op SUBQ [a, b]]
addq a b = tell [Op ADDQ [a, b]]
orq a b = tell [Op ORQ [a, b]]
xorq a b = tell [Op XORQ [a, b]]
andq a b = tell [Op ANDQ [a, b]]
cmpq a b = tell [Op CMPQ [a, b]]

pushq, popq :: MonadWriter Prog m => Operand -> m ()
pushq a = tell [Op PUSHQ [a]]
popq a = tell [Op POPQ [a]]

jmp, jz, je, jne, callq :: MonadWriter Prog m => Operand -> m ()
jmp l = tell [Op JMP [l]]
jz l = tell [Op JZ [l]]
je l = tell [Op JE [l]]
jne l = tell [Op JNE [l]]
callq l = tell [Op CALLQ [l]]

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
  Static lbl -> lbl ++ "@GOTPCREL" ++ "(%" ++ show RIP ++ ")"
  Imm i -> "$" ++ show i
  ImmL l -> "$" ++ l
  Lbl l -> l

prettyInstr :: Instr -> String
prettyInstr = \case
  -- TODO: cringe
  Op CALLQ [Reg r] ->
    "    " ++ show CALLQ ++ " " ++ ("*" ++ prettyOperand (Reg r))
  Op op ops ->
    "    " ++ show op ++ " " ++ List.intercalate ", " [prettyOperand o | o <- ops]
  Label l -> l ++ ":"
  Set l i -> ".set " ++ l ++ ", " ++ show i
  Meta m -> m

prettyProg :: Prog -> String
prettyProg = unlines . map prettyInstr