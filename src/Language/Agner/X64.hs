{-# LANGUAGE RecursiveDo #-}

module Language.Agner.X64 (Ex(..), Prog, prettyProg, compile) where

import Data.Zipper (Zipper)
import Data.Zipper qualified as Zipper
import Data.List qualified as List
import Data.Foldable (traverse_, for_)
import Data.Traversable (for)
import Data.Bits (shiftL, (.|.))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Control.Monad.State.Strict
import Control.Monad.Writer

import Control.Lens
import Data.Generics.Labels

import GHC.Generics (Generic)

import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Value (Value)
import Language.Agner.Value qualified as Value
import Language.Agner.SM qualified as SM

# define WORD_SIZE 8
# define TAG_SIZE 3
# define TAG_MASK 0b111
# define NUMBER_TAG 0b000
# define UNBOUND_TAG 0b001

data Ex
  deriving stock (Show)

type Label = String

data Op
  = MOVQ
  | ANDQ | TESTQ | CMPQ
  | SUBQ | ADDQ
  | PUSHQ | POPQ
  | JZ | JNZ | JNE | JE | JMP
  | RETQ | SYSCALL
  deriving stock (Show)

data Reg = RAX | RBX | RCX | RDX | RSI | RDI | RSP | RBP
  deriving stock (Show, Enum, Bounded)

[rax, rbx, rcx, rdx, rsi, rdi, rsp, rbp] = [Reg r | r <- [RAX ..]]

data Operand
  = Reg Reg -- RAX
  | MemReg Int Reg -- -8(%RSP)
  | MemRegL String Reg -- x(%RSP)
  | Imm Integer -- $8
  | ImmL String -- $x
  | Lbl Label -- x

instance Num Operand where
  fromInteger = Imm
  (+) = undefined; (*) = undefined; abs = undefined; signum = undefined; negate = undefined
  
data Instr
  = Op Op [Operand]
  | Label Label
  | Set String Int
  | Meta String

type Prog = [Instr]

data CompileState = MkCompileState
  { maxAllocated :: Int
  , stack :: Zipper Operand
  , uuid :: Int
  , strings :: Map String String
  }
  deriving stock (Generic)

mkStack :: [Syntax.Var] -> Zipper Operand
mkStack vars = Zipper.fromList (regs ++ onStack)
  where
    regs = [Reg r | r <- [RDX, RSI, RDI]]
    onStack = [MemReg (-WORD_SIZE * i) RBP | i <- [start ..]]
    start = length vars + 1

_alloc :: M Operand
_alloc = do
  result <- #stack %%= Zipper.next
  #maxAllocated %= max (getAllocated result)
  pure result
  where
    getAllocated (MemReg n RBP) = -n
    getAllocated _ = 0

_pop :: M Operand
_pop = #stack %%= Zipper.prev

_uuid :: M Int
_uuid = #uuid <<+= 1

_string :: String -> String -> M Operand
_string name value = do
  i <- _uuid
  let lbl = "__strings." ++ name ++ "." ++ show i
  #strings %= Map.insert lbl value
  pure (Lbl (lbl ++ "@GOTPCREL(%RIP)"))

newLabel :: String -> M Label
newLabel name = do
  i <- _uuid
  let lbl = "__lbl." ++ name ++ "." ++ show i
  tell [Label lbl]
  pure lbl

labelBlock :: String -> M () -> M Label
labelBlock name action = do
  lbl <- newLabel name
  action
  pure lbl

movq, subq, addq, andq, testq, cmpq :: Operand -> Operand -> M ()
movq a b = tell [Op MOVQ [a, b]]
subq a b = tell [Op SUBQ [a, b]]
addq a b = tell [Op ADDQ [a, b]]
andq a b = tell [Op ANDQ [a, b]]
cmpq a b = tell [Op CMPQ [a, b]]
testq a b = tell [Op TESTQ [a, b]]

pushq, popq :: Operand -> M ()
pushq a = tell [Op PUSHQ [a]]
popq a = tell [Op POPQ [a]]

jz, jnz, jne, je, jmp :: Label -> M ()
jz l = tell [Op JZ [Lbl l]]
jnz l = tell [Op JNZ [Lbl l]]
je l = tell [Op JE [Lbl l]]
jne l = tell [Op JNE [Lbl l]]
jmp l = tell [Op JMP [Lbl l]]

retq, syscall :: M ()
retq = tell [Op RETQ []]
syscall = tell [Op SYSCALL []]

type M = StateT CompileState (Writer Prog)

execM :: M a -> Prog
execM = execWriter . flip runStateT emptyState
  where
    emptyState = MkCompileState{maxAllocated = 0, stack = Zipper.empty, uuid = 0, strings = Map.empty}

_throw :: String -> M ()
_throw message = do
  str <- _string "_" (message ++ "\\n")
  movq str rsi
  movq (Imm (fromIntegral (length message + 1))) rdx
  jmp throwLabel

compileBinOp :: Syntax.BinOp -> M ()
compileBinOp = \case
  (Syntax.:+) -> mdo
    a <- _pop
    b <- _pop

    movq a rax
    andq TAG_MASK rax
    cmpq NUMBER_TAG rax
    jne whenWrongArguments

    movq b rax
    andq TAG_MASK rax
    cmpq NUMBER_TAG rax
    jne whenWrongArguments

    jmp body

    whenWrongArguments <- labelBlock "binOp.when_wrong_arguments" do
      _throw "error: wrong arguments for +"

    body <- labelBlock "binOp.body" do
      movq a rax
      addq b rax
      r <- _alloc
      movq rax r
    
    pure ()

mkSizeOfLocalsLabel :: String -> Label
mkSizeOfLocalsLabel name = ".allocate"

mkEnterLabel :: String -> Label
mkEnterLabel name = "_" ++ name

mkVarName :: Syntax.Var -> String
mkVarName var = "." ++ var

mkVarOperand :: Syntax.Var -> Operand
mkVarOperand var = MemRegL (mkVarName var) RBP

throwLabel :: String
throwLabel = "__throw"

mkIntegerOperand :: Integer -> Operand
mkIntegerOperand i = Imm (i `shiftL` TAG_SIZE .|. NUMBER_TAG)

compileInstr :: SM.Instr -> M ()
compileInstr = \case

  SM.PUSH_I i -> do
    d <- _alloc
    movq (mkIntegerOperand i) d

  SM.BINOP op -> do
    compileBinOp op

  SM.DUP -> do
    x <- _pop
    movq x rax
    a <- _alloc
    b <- _alloc
    movq rax a
    movq rax b

  SM.DROP -> do
    void _pop

  SM.ENTER name vars -> do
    tell [Label (mkEnterLabel name)]

    pushq rbp
    movq rsp rbp

    #maxAllocated .= length vars * WORD_SIZE
    #stack .= mkStack vars
    subq (ImmL (mkSizeOfLocalsLabel name)) rsp

    for_ (zip vars [1..]) \(var, i) -> do
      tell [Set (mkVarName var) (-i * WORD_SIZE)]
      movq UNBOUND_TAG (mkVarOperand var)

  SM.LEAVE name -> do
    s <- _pop
    movq s rax

    movq rbp rsp
    popq rbp

    toAllocate <- use #maxAllocated
    tell [Set (mkSizeOfLocalsLabel name) toAllocate]

  SM.RET -> do
    retq

  SM.LOAD var -> mdo
    movq (mkVarOperand var) rax
    andq TAG_MASK rax
    cmpq UNBOUND_TAG rax
    je whenUnbound

    whenBound <- labelBlock "load.when_bound" do
      movq (mkVarOperand var) rax
      movq rax =<< _alloc
      jmp done

    whenUnbound <- labelBlock "load.when_unbound" do
      _throw "error: unbound variable"

    done <- newLabel "load.done"
    pure ()

  SM.MATCH_VAR var -> mdo
    value <- _pop
    
    movq (mkVarOperand var) rax
    andq TAG_MASK rax
    cmpq UNBOUND_TAG rax
    je whenUnbound

    whenBound <- labelBlock "match_var.when_bound" mdo 
      movq value rax
      movq (mkVarOperand var) rcx
      cmpq rax rcx
      jne whenNoMatch

      whenMatch <- labelBlock "match_var.when_bound.when_match" do
        jmp matchDone

      whenNoMatch <- labelBlock "match_var.when_bound.when_no_match" do
        _throw ("error: no match variable " ++ var)

      matchDone <- newLabel "match_var.when_bound.done"
      jmp done

    whenUnbound <- labelBlock "match_var.when_unbound" do
      movq value rax
      movq rax (mkVarOperand var)
      jmp done

    done <- newLabel "match_var.done"
    pure ()

  SM.MATCH_I i -> mdo
    value <- _pop
    movq (mkIntegerOperand i) rax
    cmpq value rax
    jne whenNoMatch
    jmp whenMatch
    
    whenNoMatch <- labelBlock "match_i.when_match" do
      _throw ("error: no match integer " ++ show i)

    whenMatch <- newLabel "match_i.when_match"
    pure ()

compileProg :: SM.Prog -> M ()
compileProg prog = do
  buildFail
  traverse_ compileInstr prog
  where
    buildFail = do
      tell [Label throwLabel]

      -- print message from RSI/RDX
      movq 0x2000004 rax
      movq 1 rdi
      syscall

      -- exit
      movq 0x2000001 rax
      movq 1 rdi
      syscall

prettyOperand :: Operand -> String
prettyOperand = \case
  Reg reg -> "%" ++ show reg
  MemReg offset reg -> show offset ++ "(%" ++ show reg ++ ")"
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

compile :: SM.Prog -> Prog
compile prog = execM do
  tell [Meta ".text"]
  tell [Meta ".globl _main"]

  compileProg prog

  tell [Meta ".data"]
  strings <- use #strings
  for_ (Map.toList strings) \(l, s) -> do
    tell [Meta (l ++ ": .asciz \"" ++ s ++ "\"")]