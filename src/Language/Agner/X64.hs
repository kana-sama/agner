# include "../../../runtime/tags.h"

module Language.Agner.X64 (Ex(..), Prog, Target(..), prettyProg, compile) where

import Language.Agner.Prelude

import Data.X64

import Data.Zipper (Zipper)
import Data.Zipper qualified as Zipper
import Data.List qualified as List
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
import Language.Agner.Prettier qualified as Prettier
import Language.Agner.SM qualified as SM

data Ex
  deriving stock (Show)


-- DSL

data RuntimeName
  = RuntimeCallingContext

  | RuntimeStart
  | RuntimeYield

  | RuntimeAllocTuple | RuntimeFillTuple | RuntimeMatchTuple
  | RuntimeAllocCons  | RuntimeFillCons  | RuntimeMatchCons

  | ThrowBadArith
  | ThrowBadFun
  | ThrowBadArity
  | ThrowBadMatch
  | ThrowFunctionClause
  deriving stock Eq

isRuntimeVariable :: RuntimeName -> Bool
isRuntimeVariable = (`elem` variables)
  where
    variables =
      [ RuntimeCallingContext
      ]

runtimeName :: RuntimeName -> String
runtimeName = \case
  RuntimeCallingContext -> "_runtime__calling_context"


  RuntimeStart -> "_runtime__start"
  RuntimeYield -> "_runtime__yield"


  RuntimeAllocTuple -> "_runtime__alloc_tuple"
  RuntimeFillTuple -> "_runtime__fill_tuple"
  RuntimeMatchTuple -> "_runtime__match_tuple"

  RuntimeAllocCons -> "_runtime__alloc_cons"
  RuntimeFillCons -> "_runtime__fill_cons"
  RuntimeMatchCons -> "_runtime__match_cons"


  ThrowBadArith -> "_THROW_badarith"
  ThrowBadFun -> "_THROW_badfun"
  ThrowBadArity -> "_THROW_badarity"
  ThrowBadMatch -> "_THROW_badmatch"
  ThrowFunctionClause -> "_THROW_function_clause"

runtime :: WithTarget => RuntimeName -> Operand
runtime name =
  (if isRuntimeVariable name then Static else Lbl)
    (mkLabel (runtimeName name))


bifs :: [(Syntax.FunId, (String, [Syntax.Atom]))]
bifs =
  [ "agner:print/1" ~> "_agner__print"  // ["ok"]
  , "timer:sleep/1" ~> "_timer__sleep"  // ["ok", "infinite"]
  , "error/1"       ~> "_global__error" // []
  , "spawn/1"       ~> "_global__spawn" // []
  , "self/0"        ~> "_global__self"  // []
  ]
  where
    a ~> b = (a, b)
    (a, b) // c = (a, (b, c))

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

_string :: String -> M Operand
_string s = do
  i <- _uuid
  let lbl = "__string." ++ show i
  #strings %= Map.insert lbl s
  pure (Static lbl)

_atom :: Syntax.Atom -> M Operand
_atom a = do
  let lbl = "__atom." ++ a
  #atoms %= Map.insert lbl a
  pure (Static lbl)

_label :: String -> M Label
_label base = do
  i <- _uuid
  let lbl = "__" ++ base ++ "." ++ show i
  tell [Label lbl]
  pure lbl

regsForArguments :: [Reg]
regsForArguments = [RDI, RSI, RDX, RCX, R8, R9]

valuesStackReg, stackFrameReg :: Reg
valuesStackReg = R12
stackFrameReg = R13

mkSymbolStack :: Int -> Zipper Operand
mkSymbolStack bookedStackForVars = Zipper.fromList (regs ++ onStack)
  where
    regs = [Reg r | r <- []]
    onStack = [MemReg (WORD_SIZE * i) stackFrameReg | i <- [bookedStackForVars ..]]

_alloc :: M Operand
_alloc = do
  dest <- #stack %%= Zipper.next
  #requiredStackSize %= max (getAllocated dest)
  pure dest
  where
    getAllocated (MemReg n _) = n + WORD_SIZE
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
mkVarOp var = MemRegL (mkVarName var) stackFrameReg

mkArgOp :: Int -> Operand
mkArgOp ix = MemRegL (mkArgName ix) stackFrameReg

mkLabel :: WithTarget => String -> Label
mkLabel name = case ?target of
  Linux -> name
  MacOS -> "_" ++ name

mkFunName :: WithTarget => Syntax.FunId -> Label
mkFunName (Syntax.MkFunId ns f arity) = mkLabel (ns' ++ "." ++ f ++ "." ++ show arity)
  where ns' = case ns of Nothing -> "main"; Just ns -> ns

mkFunBody :: WithTarget => Syntax.FunId -> Label
mkFunBody f = mkFunName f ++ ".body"

mkFunEnd :: WithTarget => Syntax.FunId -> Label
mkFunEnd f = mkFunName f ++ ".end"

mkFunClause :: WithTarget => Syntax.FunId -> Maybe Int -> Label
mkFunClause f Nothing = mkFunName f ++ ".clause_fail"
mkFunClause f (Just i) = mkFunName f ++ ".clause" ++ show i

mkFunMeta :: WithTarget => Syntax.FunId -> Label
mkFunMeta f = mkFunName f ++ ".meta"

mkFunMetaOpt :: WithTarget => String -> Syntax.FunId -> Label
mkFunMetaOpt opt f = mkFunMeta f ++ "." ++ opt

entryPointName :: WithTarget => Label
entryPointName = mkLabel "main"

data Syscall
  = Exit

mkSyscall :: WithTarget => Syscall -> Operand
mkSyscall = \case
  Exit -> case ?target of
    Linux -> 60
    MacOS -> 0x2000001


-- Generators

_assertTag :: Integer -> Operand -> M () -> M ()
_assertTag tag op onFail = mdo
  movq op rax
  andq TAG_MASK rax
  cmpq (Imm tag) rax
  je (Lbl done)
  onFail
  done <- _label "assert_tag.done"
  pure ()

_popTag :: Integer -> (Operand -> M ()) -> M Operand
_popTag tag onFail = do
  op <- _pop
  _assertTag tag op (onFail op)
  pure op

compileBinOp :: WithTarget => Syntax.BinOp -> M ()
compileBinOp = \case
  (Syntax.:+) -> integerBinOp "+" addq
  (Syntax.:-) -> integerBinOp "-" subq
  where
    integerBinOp name operator = do
      b <- _pop
      a <- _pop

      let throw = do
            movq a rdi
            movq b rsi
            op <- _string name
            movq op rdx
            callq (runtime ThrowBadArith)

      _assertTag NUMBER_TAG a throw
      _assertTag NUMBER_TAG b throw
      
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

  SM.PUSH_FUN funid -> do
    movq (Static (mkFunName funid)) rax
    movq rax =<< _alloc

  SM.PUSH_TUPLE size -> do  
    movq (Imm (fromIntegral size)) rdi
    callq (runtime RuntimeAllocTuple)

    when (odd size) do
      subq WORD_SIZE rsp
    replicateM_ size do
      pushq =<< _pop

    movq rax =<< _alloc

    movq rax rdi
    movq (Imm (fromIntegral size)) rsi
    movq rsp rdx
    callq (runtime RuntimeFillTuple)

    addq (Imm (fromIntegral (WORD_SIZE * size))) rsp
    when (odd size) do
      addq WORD_SIZE rsp

  SM.PUSH_NIL -> do
    movq NIL_TAG =<< _alloc

  SM.PUSH_CONS -> do
    callq (runtime RuntimeAllocCons)

    tail <- _pop; pushq tail
    head <- _pop; pushq head
    movq rax =<< _alloc

    movq rax rdi
    popq rsi
    popq rdx
    callq (runtime RuntimeFillCons)

  SM.BINOP op -> do
    compileBinOp op

  SM.DROP -> do
    void _pop

  SM.DUP -> do
    s <- _pop
    movq s rax
    movq rax =<< _alloc
    movq rax =<< _alloc

  -- TODO: unify with simple call
  SM.DYN_CALL arity -> mdo
    let argsOnStack = max 0 (arity - length regsForArguments)

    -- align stack
    when (argsOnStack > 0 && odd argsOnStack) do
      subq WORD_SIZE rsp

    -- push last arguments to stack
    replicateM_ argsOnStack do
      val <- _pop
      pushq val

    -- move first arguments to regs
    for_ (reverse (take arity regsForArguments)) \reg -> do
      val <- _pop
      movq val (Reg reg)

    f <- _popTag FUN_TAG \value -> do
      movq value rdi
      callq (runtime ThrowBadFun)

    -- get arity
    movq f rax
    subq WORD_SIZE rax
    movq (MemReg 0 RAX) rax
    addq f rax
    movq (MemReg 0 RAX) rax

    cmpq (Imm (fromIntegral arity)) rax
    je (Lbl done)

    movq f rdi
    movq (Imm (fromIntegral arity)) rsi
    callq (runtime ThrowBadArity)

    done <- _label "dyn_call.done"
    movq f rax
    callq rax

    -- restore stack
    when (argsOnStack > 0) do
      let toRestore = argsOnStack + if odd argsOnStack then 1 else 0
      addq (Imm (fromIntegral toRestore * WORD_SIZE)) rsp

    -- push result to symbolic stack
    movq rax =<< _alloc

  SM.CALL f Syntax.TailCall -> do
    for_ (reverse [0 .. f.arity-1]) \i -> do
      arg <- _pop
      movq arg rax
      movq rax (mkArgOp i)
    movq UNBOUND_TAG =<< _alloc
    jmp (Lbl (mkFunBody f))

  SM.CALL f Syntax.SimpleCall -> do
    let argsOnStack = max 0 (f.arity - length regsForArguments)

    -- align stack
    when (argsOnStack > 0 && odd argsOnStack) do
      subq WORD_SIZE rsp

    -- push last arguments to stack
    replicateM_ argsOnStack do
      val <- _pop
      pushq val

    -- move first arguments to regs
    for_ (reverse (take f.arity regsForArguments)) \reg -> do
      val <- _pop
      movq val (Reg reg)

    callq (Lbl (mkFunName f))

    -- restore stack
    when (argsOnStack > 0) do
      let toRestore = argsOnStack + if odd argsOnStack then 1 else 0
      addq (Imm (fromIntegral toRestore * WORD_SIZE)) rsp

    -- push result to symbolic stack
    movq rax =<< _alloc

  SM.FUNCTION funid vars -> do
    tell [Meta "\n"]

    tell [Meta (".align " ++ show WORD_SIZE)]
    tell [Meta (".skip " ++ show FUN_TAG)]
    tell [Meta  (mkFunMetaOpt "size" funid ++ ": .quad " ++ mkFunEnd funid ++ " - " ++ mkFunName funid)]
    tell [Label (mkFunName funid)]

    -- prologue
    pushq (Reg stackFrameReg)
    movq (Reg valuesStackReg) (Reg stackFrameReg)
    addq (ImmL (mkStackSizeName funid)) (Reg valuesStackReg)

    -- enter
    let bookedStackForVars = funid.arity + length vars
    #requiredStackSize .= bookedStackForVars * WORD_SIZE
    #stack .= mkSymbolStack bookedStackForVars

    for_ (zip [0..funid.arity - 1] regsForArguments) \(argN, reg) -> do
      _set (mkArgName argN) (WORD_SIZE * argN)
      movq (Reg reg) (mkArgOp argN)

    for_ (drop (length regsForArguments) [0..funid.arity - 1]) \argN -> do
      -- 2 is return address + stack reg
      let from = WORD_SIZE * (2 + argN - length regsForArguments)
      _set (mkArgName argN) (WORD_SIZE * argN)
      movq (MemReg from RSP) rax
      movq rax (mkArgOp argN)

    for_ (zip vars [funid.arity ..]) \(var, varN) -> do
      tell [Set (mkVarName var) (WORD_SIZE * varN)]

    tell [Label (mkFunBody funid)]

  SM.YIELD funid -> do
    movq (Static (mkFunMetaOpt "name_a" funid)) rdi
    callq (runtime RuntimeYield)

  SM.FUNCTION_END funid -> do
    _set (mkStackSizeName funid) =<< use #requiredStackSize

    tell [Meta (".align " ++ show WORD_SIZE)]
    tell [Label (mkFunEnd funid)]

    tell [Label (mkFunMeta funid)]
    tell [Meta  (mkFunMetaOpt "arity" funid ++ ": .quad " ++ show funid.arity)]
    tell [Meta  (mkFunMetaOpt "name" funid ++ ": .asciz " ++ show (Prettier.string Prettier.funId funid))]
    tell [Meta  (mkFunMetaOpt "name_a" funid ++ ": .asciz " ++ show (Prettier.string Prettier.funIdA funid))]

  SM.CLAUSE funid clauseN vars -> do
    tell [Label (mkFunClause funid (Just clauseN))]

    for_ vars \var -> do
      movq UNBOUND_TAG (mkVarOp var)

    for_ (reverse [0..funid.arity - 1]) \argN -> do
      movq (mkArgOp argN) rax
      movq rax =<< _alloc

  SM.FAIL_CLAUSE funid _ -> do
    tell [Label (mkFunClause funid Nothing)]
    movq (Static (mkFunMeta funid)) rdi
    movq (Reg stackFrameReg) rsi
    callq (runtime ThrowFunctionClause)

  SM.LEAVE _ -> do
    s <- _pop
    movq s rax

    -- epilogue
    movq (Reg stackFrameReg) (Reg valuesStackReg)
    popq (Reg stackFrameReg)

  SM.RET -> do
    retq

  SM.LOAD var -> mdo
    movq (mkVarOp var) rax
    movq rax =<< _alloc

  SM.MATCH_I i onFail -> mdo
    value <- _pop
    movq value rax
    cmpq (encodeInteger i) rax
    je (Lbl when_equal)

    when_not_equal <- _label "match_i.when_not_equal"
    case onFail of
      SM.Throw -> do movq value rdi; callq (runtime ThrowBadMatch)
      SM.NextClause funid clauseN -> do
        jmp (Lbl (mkFunClause funid clauseN))

    when_equal <- _label "match_i.when_equal"
    pure ()

  SM.MATCH_ATOM atom onFail -> mdo
    value <- _pop
    movq value rax
    atom' <- _atom atom
    cmpq atom' rax
    je (Lbl when_equal)

    when_not_equal <- _label "match_i.when_not_equal"
    case onFail of
      SM.Throw -> do movq value rdi; callq (runtime ThrowBadMatch)
      SM.NextClause funid clauseN -> jmp (Lbl (mkFunClause funid clauseN))

    when_equal <- _label "match_i.when_equal"
    pure ()

  SM.MATCH_VAR var onFail -> mdo
    value <- _pop

    movq (mkVarOp var) rax
    andq UNBOUND_TAG rax
    jz (Lbl when_bound)

    when_unbound <- _label "match_var.when_unbound"
    movq value rax
    movq rax (mkVarOp var)
    jmp (Lbl done)

    when_bound <- _label "match_var.when_bound"
    mdo
      movq (mkVarOp var) rax
      cmpq rax value
      je (Lbl done)

      when_not_equal <- _label "match_var.when_not_equal"
      case onFail of
        SM.Throw -> do movq value rdi; callq (runtime ThrowBadMatch)
        SM.NextClause funid clauseN -> jmp (Lbl (mkFunClause funid clauseN))

    done <- _label "match_var.done"
    pure ()

  SM.MATCH_TUPLE size onFail -> mdo
    value <- _pop
    movq value rdi
    movq (Imm (fromIntegral size)) rsi
    callq (runtime RuntimeMatchTuple)
    cmpq 0 rax
    jne (Lbl unpack_tuple)

    case onFail of
      SM.Throw -> do movq value rdi; callq (runtime ThrowBadMatch)
      SM.NextClause funid clauseN -> jmp (Lbl (mkFunClause funid clauseN))

    unpack_tuple <- _label "match_tuple.unpack_tuple"
    movq rax rbx
    for_ [1..size] \i -> do
      movq (MemReg ((size-i) * WORD_SIZE) RBX) rax
      movq rax =<< _alloc

  SM.MATCH_NIL onFail -> mdo
    value <- _pop
    movq value rax
    cmpq NIL_TAG rax
    je (Lbl when_equal)

    case onFail of
      SM.Throw -> do movq value rdi; callq (runtime ThrowBadMatch)
      SM.NextClause funid clauseN -> jmp (Lbl (mkFunClause funid clauseN))

    when_equal <- _label "match_nil.when_equal"
    pure ()
    
  SM.MATCH_CONS onFail -> mdo
    value <- _pop
    movq value rdi
    callq (runtime RuntimeMatchCons)
    cmpq 0 rax
    jne (Lbl unpack_cons)

    case onFail of
      SM.Throw -> do movq value rdi; callq (runtime ThrowBadMatch)
      SM.NextClause funid clauseN -> jmp (Lbl (mkFunClause funid clauseN))

    unpack_cons <- _label "match_cons.unpack_cons"
    movq (MemReg (1 * WORD_SIZE) RAX) rbx
    movq rbx =<< _alloc
    movq (MemReg (0 * WORD_SIZE) RAX) rbx
    movq rbx =<< _alloc

compileProg :: WithTarget => SM.Prog -> M ()
compileProg prog = do
  for_ prog \instr -> do
    tell [Meta ("// " ++ show instr)]
    compileInstr instr
      
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

  movq (Static (mkFunName ("main" Syntax.:/ 0))) rdi
  callq (runtime RuntimeStart)

  addq WORD_SIZE rsp
  movq 0 rax
  retq

  
  -- BiFs mapping
  tell [Meta "// BiFs mapping"]
  for_ bifs \(funId, (runtimeName, context)) -> do
    tell [Label (mkFunName funId)]
    when (not (null context)) do
      movq (runtime RuntimeCallingContext) rbx
      for_ (zip context [0..]) \(a, i) -> do
        a <- _atom a
        movq a rax
        movq rax (MemReg (i * WORD_SIZE) RBX)
    jmp (Lbl (mkLabel runtimeName))

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
