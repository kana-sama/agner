{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.X64 where

import Data.List qualified as List
import Data.Char qualified as Char

type Label = String

data Op = LEAQ | MOVQ | MOVABSQ | MOVZBQ | SUBQ | ADDQ | PUSHQ | POPQ | RETQ | ANDQ | ORQ | XORQ | JMP | JMPQ | JZ | JE | JNE | JNZ | SYSCALL | CMPQ | CMPB | CALLQ | UD2
  deriving stock (Show, Eq)

data Reg
  = RAX | RBX | RCX | RDX | RSI | RDI | RSP | RBP | RIP | R8 | R9 | R10 | R11 | R12 | R13
  | AL
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data Operand
  = Reg Reg -- RAX
  | MemReg ~Int Reg -- -8(%RSP)
  | MemRegL Label Reg -- qwe(%RBP)
  | Static Label -- lbl@GOTPCREL(%RIP)
  | Imm Int -- $8
  | ImmL String
  | Lbl Label
  | Indirect Reg
  deriving stock (Eq, Ord, Show)

instance Num Operand where
  fromInteger = Imm . fromIntegral
  (+) = undefined; (-) = undefined; (*) = undefined; abs = undefined; signum = undefined

data Instr
  = Op Op [Operand]
  | Meta String

type Prog = [Instr]

class FromReg a where fromReg :: Reg -> a
instance FromReg Reg where fromReg = id
instance FromReg Operand where fromReg = Reg


[rax, rbx, rcx, rdx, rsi, rdi, rsp, rbp, rip, r8, r9, r10, r11, r12, r13, al] = [fromReg r | r <- [RAX ..]]

leaq a b = Op LEAQ [a, b]
movq a b = Op MOVQ [a, b]
movabsq a b = Op MOVABSQ [a, b]
movzbq a b = Op MOVZBQ  [a, b]
subq a b = Op SUBQ [a, b]
addq a b = Op ADDQ [a, b]
orq a b = Op ORQ [a, b]
xorq a b = Op XORQ [a, b]
andq a b = Op ANDQ [a, b]
cmpq a b = Op CMPQ [a, b]
cmpb a b = Op CMPB [a, b]

pushq a = Op PUSHQ [a]
popq a = Op POPQ [a]

jmp l = Op JMP [Lbl l]
jmpq l = Op JMPQ [l]
jz l = Op JZ [Lbl l]
je l = Op JE [Lbl l]
jne l = Op JNE [Lbl l]
callq l = Op CALLQ [Lbl l]
callq' l = Op CALLQ [l]

retq = Op RETQ []
syscall = Op SYSCALL []
ud2 = Op UD2 []

_newline = Meta ""
_set lbl value = Meta (".set " ++ lbl ++ ", " ++ value)
_label lbl = Meta (lbl ++ ":")
_text = Meta ".text"
_data = Meta ".data"
_globl lbl = Meta (".globl " ++ lbl)
_align i = Meta (".align " ++ show i)
_skip i = Meta (".skip " ++ show i)
_quad s = Meta (".quad " ++ s)
_asciz s = Meta (".asciz " ++ s)
_comment comm = Meta ("\n// " ++ comm)
_include path = Meta ("#include " ++ show path)

prettyOperand :: Operand -> String
prettyOperand = \case
  Reg reg -> "%" ++ lower (show reg)
  MemReg offset reg -> (if offset > 0 then "+" else "") ++ show offset ++ "(%" ++ lower (show reg) ++ ")"
  MemRegL lbl reg -> lbl ++ "(%" ++ lower (show reg) ++ ")"
  Static lbl -> lbl ++ "@GOTPCREL" ++ "(%" ++ show RIP ++ ")"
  Imm i -> "$" ++ show i
  ImmL l -> "$" ++ l
  Lbl l -> l
  Indirect reg -> "*%" ++ lower (show reg)

prettyInstr :: Instr -> String
prettyInstr = \case
  Op op ops ->
    "    " ++ lower (show op) ++ " \t" ++ List.intercalate ", " [prettyOperand o | o <- ops]
  Meta m -> m

lower :: String -> String
lower = map Char.toLower

prettyProg :: Prog -> String
prettyProg = unlines . map prettyInstr
