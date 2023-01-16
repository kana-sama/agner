# define WORD_SIZE 8

module Language.Agner.X64 (Ex(..), Prog, prettyProg, compile) where

import Data.X64

import Data.Zipper (Zipper)
import Data.Zipper qualified as Zipper
import Data.Foldable (traverse_, for_)
import Data.Traversable (for)
import Data.List qualified as List

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
  } deriving stock (Generic)

execM :: M a -> Prog
execM = execWriter . flip runStateT emptyState
  where emptyState = MkCompileState{requiredStackSize = 0, stack = Zipper.empty}

_enter :: M ()
_enter = do
  #requiredStackSize .= 0
  #stack .= Zipper.fromList (regs ++ onStack)
  where
    regs = [Reg r | r <- [RBX, RCX, RDX, RSI, RDI]]
    onStack = [MemReg (-WORD_SIZE * i) RBP | i <- [1 ..]]

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

compileInstr :: SM.Instr -> M ()
compileInstr = \case

  SM.PUSH_I x -> do
    d <- _alloc
    movq (Imm x) d

  SM.BINOP op -> do
    compileBinOp op

  SM.DROP -> do
    void _pop

  SM.ENTER name -> do
    tell [Label ("_" ++ name)]

    pushq rbp
    movq rsp rbp

    _enter
    subq (ImmL stackSizeName) rsp

  SM.LEAVE name -> do
    s <- _pop
    movq s rax

    movq rbp rsp
    popq rbp

    requiredStackSize <- use #requiredStackSize
    tell [Set stackSizeName requiredStackSize]

  SM.RET -> do
    retq

compileProg :: SM.Prog -> M ()
compileProg prog =
  traverse_ compileInstr prog

compile :: SM.Prog -> Prog
compile prog = execM do
  tell [Meta ".text"]
  tell [Meta ".globl _main"]

  compileProg prog