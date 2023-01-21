# define WORD_SIZE 8
# define TAG_SIZE 3
# define TAG_MASK 0b111
# define NUMBER_TAG 0b000
# define UNBOUND_TAG 0b001

module Language.Agner.X64 (Ex(..), Prog, prettyProg, compile) where

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
  } deriving stock (Generic)

execM :: M a -> Prog
execM = execWriter . flip runStateT emptyState
  where
    emptyState =
      MkCompileState
        { requiredStackSize = 0
        , stack = Zipper.empty
        , uuid = 0
        , strings = Map.empty
        }

_uuid :: M Int
_uuid = #uuid <<+= 1

_string :: String -> M Operand
_string s = do
  i <- _uuid
  let lbl = "__string." ++ show i
  #strings %= Map.insert lbl s
  pure (MemRegL (lbl ++ "@GOTPCREL") RIP)

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

-- Generators

compileBinOp :: Syntax.BinOp -> M ()
compileBinOp = \case
  (Syntax.:+) -> do
    b <- _pop
    a <- _pop
    movq a rax
    addq b rax
    r <- _alloc
    movq rax r

encodeInteger :: Integer -> Operand
encodeInteger i = Imm (i `shiftL` TAG_SIZE .|. NUMBER_TAG)

compileInstr :: SM.Instr -> M ()
compileInstr = \case

  SM.PUSH_I x -> do
    d <- _alloc
    movq (encodeInteger x) d

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
    tell [Label ("_" ++ name)]

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
    a <- _pop
    movq a rax
    cmpq (encodeInteger i) rax
    je when_equal

    when_not_equal <- _label "match_i.when_not_equal"
    _throw ("No match " ++ show i)

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

compileProg :: SM.Prog -> M ()
compileProg prog = do
  traverse_ compileInstr prog
  
  tell [Label "__throw"]
  movq 0x2000004 rax
  movq 1 rdi
  syscall

  movq 0x2000001 rax
  movq 1 rbx
  syscall

compile :: SM.Prog -> Prog
compile prog = execM do
  tell [Meta ".text"]
  tell [Meta ".globl _main"]

  compileProg prog

  tell [Meta ".data"]
  strings <- use #strings
  for_ (Map.toList strings) \(lbl, str) -> do
    tell [Meta (lbl ++ ": .ascii " ++ show str)]
