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

import Text.Show.Unicode (ushow)

import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Value (Value)
import Language.Agner.Value qualified as Value
import Language.Agner.SM qualified as SM

data Ex
  deriving stock (Show)


-- DSL

data BiFContext
  = NoContext
  | WithAtomContext Syntax.Atom

bifs :: [(Syntax.FunId, (String, BiFContext))]
bifs =
  [ "agner:print/1" ~> ("_agner__print", WithAtomContext "ok")
  ]
  where
    name ~> runtime = (name, runtime)

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

regsForArguments :: [Reg]
regsForArguments = [RDI, RSI, RDX, RCX, R8, R9]

mkSymbolStack :: Int -> Zipper Operand
mkSymbolStack bookedStackForVars = Zipper.fromList (regs ++ onStack)
  where
    regs = [Reg r | r <- [RBX]]
    onStack = [MemReg (-WORD_SIZE * i) RBP | i <- [bookedStackForVars + 1 ..]]

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

mkStackSizeName :: WithTarget => Syntax.FunId -> Label
mkStackSizeName funid = mkFunName funid ++ ".stack_size"

mkVarName :: Syntax.Var -> Label
mkVarName var = "." ++ var

mkArgName :: Int -> Label
mkArgName ix = "." ++ "arg" ++ show ix

mkVarOp :: Syntax.Var -> Operand
mkVarOp var = MemRegL (mkVarName var) RBP

mkArgOp :: Int -> Operand
mkArgOp ix = MemRegL (mkArgName ix) RBP

mkLabel :: WithTarget => String -> Label
mkLabel name = case ?target of
  Linux -> name
  MacOS -> "_" ++ name

mkFunName :: WithTarget => Syntax.FunId -> Label
mkFunName (Syntax.MkFunId ns f arity) = mkLabel (ns' ++ "." ++ f ++ "." ++ show arity)
  where ns' = case ns of Nothing -> "main"; Just ns -> ns

mkFunClause :: WithTarget => Syntax.FunId -> Maybe Int -> Label
mkFunClause f Nothing = mkFunName f ++ ".clause_fail"
mkFunClause f (Just i) = mkFunName f ++ ".clause" ++ show i

entryPointName :: WithTarget => Label
entryPointName = mkLabel "main"

callContextOp :: WithTarget => Operand
callContextOp = mkStaticOperand (mkLabel "RUNTIME_call_context")

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

_popInteger :: String -> M Operand
_popInteger msg = do
  op <- _pop
  _assertInteger (msg ++ ", integer was expected") op
  pure op

compileBinOp :: Syntax.BinOp -> M ()
compileBinOp = \case
  (Syntax.:+) -> integerBinOp "+" addq
  (Syntax.:-) -> integerBinOp "-" subq
  where
    integerBinOp name operator = do
      b <- _popInteger ("bad args for " ++ name)
      a <- _popInteger ("bad args for " ++ name)
      movq a rax
      operator b rax
      movq rax =<< _alloc

encodeInteger :: Integer -> Operand
encodeInteger i = Imm (i `shiftL` TAG_SIZE .|. NUMBER_TAG)

compileInstr :: WithTarget => SM.Instr -> M ()
compileInstr = \case

  SM.PUSH_I x -> do
    movq (encodeInteger x) =<< _alloc

  SM.PUSH_ATOM a -> do
    a <- _atom a
    movq a rax
    movq rax =<< _alloc

  SM.BINOP op -> do
    compileBinOp op

  SM.DROP -> do
    void _pop

  SM.DUP -> do
    s <- _pop
    movq s rax
    movq rax =<< _alloc
    movq rax =<< _alloc

  SM.CALL f -> do
    let argsOnStack = max 0 (f.arity - length regsForArguments)

    -- align stack
    when (argsOnStack > 0 && odd argsOnStack) do
      subq WORD_SIZE rsp

    -- push last arguments to stack
    replicateM argsOnStack do
      val <- _pop
      pushq val

    -- move first arguments to regs
    for_ (reverse (take f.arity regsForArguments)) \reg -> do
      val <- _pop
      movq val (Reg reg)

    callq (mkFunName f)

    -- restore stack
    when (argsOnStack > 0) do
      let toRestore = argsOnStack + if odd argsOnStack then 1 else 0
      addq (Imm (fromIntegral toRestore * WORD_SIZE)) rsp

    -- push result to symbolic stack
    movq rax =<< _alloc

  SM.FUNCTION funid vars -> do
    tell [Meta "\n"]

    tell [Label (mkFunName funid)]

    -- prologue
    pushq rbp
    movq rsp rbp
    subq (ImmL (mkStackSizeName funid)) rsp

    -- enter
    let bookedStackForVars = funid.arity + length vars
    #requiredStackSize .= bookedStackForVars * WORD_SIZE
    #stack .= mkSymbolStack bookedStackForVars

    for_ (zip [1..funid.arity] regsForArguments) \(argN, reg) -> do
      _set (mkArgName argN) (-WORD_SIZE * argN)
      movq (Reg reg) (mkArgOp argN)

    for_ (drop (length regsForArguments) [1..funid.arity]) \argN -> do
      -- 2 is return address + RBP
      let from = WORD_SIZE * (2 + (argN-1) - length regsForArguments)
      _set (mkArgName argN) (WORD_SIZE * argN)
      movq (MemReg from RBP) rax
      movq rax (mkArgOp argN)

    for_ (zip vars [funid.arity + 1 ..]) \(var, i) -> do
      tell [Set (mkVarName var) (-WORD_SIZE * i)]

  SM.FUNCTION_END funid -> do
    requiredStackSize <- use #requiredStackSize
    let isAligned = requiredStackSize `mod` (WORD_SIZE * 2) == 0
    _set (mkStackSizeName funid) (requiredStackSize + if isAligned then 0 else WORD_SIZE)

  SM.CLAUSE funid clauseN vars -> do
    tell [Label (mkFunClause funid (Just clauseN))]

    for_ vars \var -> do
      movq UNBOUND_TAG (mkVarOp var)

    for_ (reverse [1..funid.arity]) \argN -> do
      movq (mkArgOp argN) rax
      movq rax =<< _alloc

  SM.FAIL_CLAUSE funid _ -> do
    tell [Label (mkFunClause funid Nothing)]
    _throw ("No function clause matching " ++ show funid)

  SM.LEAVE _ -> do
    s <- _pop
    movq s rax

    -- epilogue
    movq rbp rsp
    popq rbp

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

  SM.MATCH_I i onFail -> mdo
    value <- _pop
    movq value rax
    cmpq (encodeInteger i) rax
    je when_equal

    when_not_equal <- _label "match_i.when_not_equal"
    case onFail of
      SM.Throw -> _throw ("No match " ++ show i)
      SM.NextClause funid clauseN -> jmp (mkFunClause funid clauseN)

    when_equal <- _label "match_i.when_equal"
    pure ()

  SM.MATCH_ATOM atom onFail -> mdo
    value <- _pop
    movq value rax
    atom' <- _atom atom
    cmpq atom' rax
    je when_equal

    when_not_equal <- _label "match_i.when_not_equal"
    case onFail of
      SM.Throw -> _throw ("No match " ++ atom)
      SM.NextClause funid clauseN -> jmp (mkFunClause funid clauseN)

    when_equal <- _label "match_i.when_equal"
    pure ()

  SM.MATCH_VAR var onFail -> mdo
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
      case onFail of
        SM.Throw -> _throw ("No match " ++ show var)
        SM.NextClause funid clauseN -> jmp (mkFunClause funid clauseN)

    done <- _label "match_var.done"
    pure ()

_throw :: String -> M ()
_throw msg = do
  str <- _string msg
  movq str rdi
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

  callq (mkFunName ("main" Syntax.:/ 0))
  movq rax rdi
  callq (mkLabel "_print_value")

  addq WORD_SIZE rsp
  movq 0 rax
  retq

  tell [Label "__throw"]
  callq (mkLabel "_print_error_message")

  movq (mkSyscall Exit) rax
  movq 1 rdi
  syscall

  
  -- BiFs mapping
  tell [Meta "// BiFs mapping"]
  for_ bifs \(funId, (runtimeName, context)) -> do
    tell [Label (mkFunName funId)]
    case context of
      NoContext -> pure ()
      WithAtomContext a -> do
        a <- _atom a
        movq a rax
        movq callContextOp rbx
        movq rax (MemReg 0 RBX)
    jmp (mkLabel runtimeName)

  -- data
  tell [Meta ".data"]

  strings <- use #strings
  for_ (Map.toList strings) \(lbl, str) -> do
    tell [Meta (lbl ++ ": .asciz " ++ ushow str)]

  atoms <- use #atoms
  for_ (Map.toList atoms) \(lbl, str) -> do
    tell [Meta (".align " ++ show WORD_SIZE)]
    tell [Meta (".skip " ++ show ATOM_TAG)]
    tell [Meta (lbl ++ ": .asciz " ++ show str)]
