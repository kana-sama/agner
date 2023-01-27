{-# LANGUAGE PartialTypeSignatures #-}
# include "./X64.h"

module Language.Agner.X64 (Ex(..), Prog, Target(..), prettyProg, compile) where

import Data.X64

import Data.Zipper (Zipper)
import Data.Zipper qualified as Zipper
import Data.Foldable (traverse_, for_)
import Data.Traversable (for)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Bits (shiftL, (.|.))

import Control.Monad.State.Strict
import Control.Monad.Writer

import Control.Lens
import Data.Generics.Labels
import GHC.Generics (Generic)

import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Value (Value)
import Language.Agner.Value qualified as Value
import Language.Agner.SM qualified as SM

data Ex
  deriving stock (Show)


-- DSL

type M = StateT CompileState (Writer Prog)
data CompileState = MkCompileState
  { requiredStackSize :: Int
  , stack :: Zipper Operand
  , uuid :: Int
  , strings :: Map Label String
  , atoms :: Map Label Syntax.Atom
  } deriving stock (Generic)

data Target = Linux | MacOS

type WithTarget = ?target :: Target

execM :: M a -> Prog
execM = execWriter . flip runStateT emptyState
  where
    emptyState =
      MkCompileState
        { requiredStackSize = 0
        , stack = Zipper.empty
        , uuid = 0
        , strings = Map.empty
        , atoms = Map.empty
        }

_uuid :: M Int
_uuid = #uuid <<+= 1

mkStaticOperand :: Label -> Operand
mkStaticOperand lbl = MemRegL (lbl ++ "@GOTPCREL") RIP

_string :: String -> M Operand
_string s = do
  i <- _uuid
  let lbl = "__string." ++ show i
  #strings %= Map.insert lbl s
  pure (mkStaticOperand lbl)

_atom :: Syntax.Atom -> M Operand
_atom a = do
  let lbl = "__atom." ++ a
  #atoms %= Map.insert lbl a
  pure (mkStaticOperand lbl)

_label :: String -> M Label
_label base = do
  i <- _uuid
  let lbl = "__" ++ base ++ "." ++ show i
  tell [Label lbl]
  pure lbl

_enter :: [Syntax.Var] -> M ()
_enter vars = do
  #requiredStackSize .= length vars * WORD_SIZE
  #stack .= Zipper.fromList (regs ++ onStack)
  where
    regs = [Reg r | r <- [RBX, RCX, RDX, RSI, RDI]]
    onStack = [MemReg (-WORD_SIZE * i) RBP | i <- [length vars + 1 ..]]

_alloc :: M Operand
_alloc = do
  dest <- #stack %%= Zipper.next
  #requiredStackSize %= max (getAllocated dest)
  pure dest
  where
    getAllocated (MemReg n RBP) = -n
    getAllocated _ = 0

_pop :: M Operand
_pop = #stack %%= Zipper.prev


-- Names

stackSizeName :: Label
stackSizeName = ".stack_size"

mkVarName :: Syntax.Var -> Label
mkVarName var = "." ++ var

mkVarOp :: Syntax.Var -> Operand
mkVarOp var = MemRegL (mkVarName var) RBP

mkFunName :: WithTarget => String -> Label
mkFunName funName = case ?target of
  Linux -> funName
  MacOS -> "_" <> funName

entryPointName :: WithTarget => Label
entryPointName = mkFunName "main"

data Syscall = Write | Exit

mkSyscall :: WithTarget => Syscall -> Operand
mkSyscall = \case
  Write -> case ?target of
    Linux -> 1
    MacOS -> 0x2000004
  Exit -> case ?target of
    Linux -> 60
    MacOS -> 0x2000001


-- Generators

_assertInteger :: String -> Operand -> M ()
_assertInteger errorMsg op = mdo
  movq op rax
  andq TAG_MASK rax
  cmpq NUMBER_TAG rax
  je done

  _throw errorMsg

  done <- _label "assert_integer.done"
  pure ()


compileBinOp :: Syntax.BinOp -> M ()
compileBinOp = \case
  (Syntax.:+) -> do
    b <- _pop
    a <- _pop

    _assertInteger "bad args for +, integer was expected" a
    _assertInteger "bad args for +, integer was expected" b

    movq a rax
    addq b rax
    r <- _alloc
    movq rax r

encodeInteger :: Integer -> Operand
encodeInteger i = Imm (i `shiftL` TAG_SIZE .|. NUMBER_TAG)

compileInstr :: WithTarget => SM.Instr -> M ()
compileInstr = \case

  SM.PUSH_I x -> do
    movq (encodeInteger x) =<< _alloc

  SM.PUSH_ATOM a -> do
    a <- _atom a
    movq a =<< _alloc

  SM.BINOP op -> do
    compileBinOp op

  SM.DROP -> do
    void _pop

  SM.DUP -> do
    s <- _pop
    movq s rax
    movq rax =<< _alloc
    movq rax =<< _alloc

  SM.ENTER name vars -> do
    tell [Label (mkFunName name)]

    pushq rbp
    movq rsp rbp

    _enter vars
    subq (ImmL stackSizeName) rsp
    for_ (zip vars [1 ..]) \(var, i) -> do
      tell [Set (mkVarName var) (-WORD_SIZE * i)]
      movq UNBOUND_TAG (mkVarOp var)

  SM.LEAVE name -> do
    s <- _pop
    movq s rax

    movq rbp rsp
    popq rbp

    requiredStackSize <- use #requiredStackSize
    tell [Set stackSizeName requiredStackSize]

  SM.RET -> do
    retq

  SM.LOAD var -> mdo
    movq (mkVarOp var) rax
    andq UNBOUND_TAG rax
    jz when_bound

    when_unbound <- _label "load.when_unbound"
    _throw ("Unbound variable " ++ var)

    when_bound <- _label "load.when_bound"
    movq (mkVarOp var) rax
    movq rax =<< _alloc

  SM.MATCH_I i -> mdo
    value <- _pop
    movq value rax
    cmpq (encodeInteger i) rax
    je when_equal

    when_not_equal <- _label "match_i.when_not_equal"
    _throw ("No match " ++ show i)

    when_equal <- _label "match_i.when_equal"
    pure ()

  SM.MATCH_ATOM atom -> mdo
    value <- _pop
    movq value rax
    atom' <- _atom atom
    cmpq atom' rax
    je when_equal

    when_not_equal <- _label "match_i.when_not_equal"
    _throw ("No match " ++ atom)

    when_equal <- _label "match_i.when_equal"
    pure ()

  SM.MATCH_VAR var -> mdo
    val <- _pop

    movq (mkVarOp var) rax
    andq UNBOUND_TAG rax
    jz when_bound

    when_unbound <- _label "match_var.when_unbound"
    movq val rax
    movq rax (mkVarOp var)
    jmp done

    when_bound <- _label "match_var.when_bound"
    mdo
      movq (mkVarOp var) rax
      cmpq rax val
      je done

      when_not_equal <- _label "match_var.when_not_equal"
      _throw ("No match " ++ show var)

    done <- _label "match_var.done"
    pure ()

_throw :: String -> M ()
_throw msg = do
  let msg' = msg ++ "\n"
  str <- _string msg'
  movq str rsi
  movq (Imm (fromIntegral (length msg'))) rdx
  jmp "__throw"

compileProg :: WithTarget => SM.Prog -> M ()
compileProg = traverse_ compileInstr
      
compile :: Target -> SM.Prog -> Prog
compile target prog = let ?target = target in execM do
  -- header
  tell [Meta ".text"]
  tell [Meta (".globl " <> entryPointName)]


  -- prog
  compileProg prog


  -- entry point
  tell [Label entryPointName]
  subq WORD_SIZE rsp

  call (mkFunName "prog")
  movq rax rdi
  call (mkFunName "_print_value")

  addq WORD_SIZE rsp
  movq 0 rax
  retq

  
  -- throw
  tell [Label "__throw"]
  movq (mkSyscall Write) rax
  movq 1 rdi
  syscall

  movq (mkSyscall Exit) rax
  movq 1 rdi
  syscall
  

  -- data
  tell [Meta ".data"]

  strings <- use #strings
  for_ (Map.toList strings) \(lbl, str) -> do
    tell [Meta (lbl ++ ": .ascii " ++ show str)]

  atoms <- use #atoms
  for_ (Map.toList atoms) \(lbl, str) -> do
    tell [Meta (".align " ++ show WORD_SIZE)]
    tell [Meta (".skip " ++ show ATOM_TAG)]
    tell [Meta (lbl ++ ": .asciz " ++ show str)]
