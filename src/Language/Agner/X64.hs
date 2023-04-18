# include "../../../runtime/tags.h"

{-# LANGUAGE TypeFamilies, RecursiveDo #-}

module Language.Agner.X64New (module Language.Agner.X64New, module Data.X64) where

import GHC.Exts (IsList(..))

import Data.X64
import Data.Set qualified as Set
import Data.List qualified as List
import Data.Map.Strict qualified as Map

import Language.Agner.Prelude
import Language.Agner.Syntax


data Target = Linux | MacOS
type WithTarget = ?target :: Target

data Ctx = MkCtx
  { uuid :: Int
  , allocated :: Set Operand
  , required_slots :: Int
  , stackframe :: ~Int
  , variables :: Map Var Operand
  , atoms :: Set Atom
  } deriving stock (Generic)
  
type M = StateT Ctx (Writer Prog)

defaultStackFrame :: Int
defaultStackFrame = error "required_slots is not evaluated yet"

runM :: M () -> Prog
runM m = execWriter (evalStateT m initialCtx) where
  initialCtx = MkCtx
    { uuid = 0
    , allocated = Set.empty
    , required_slots = 0
    , stackframe = defaultStackFrame
    , variables = Map.empty
    , atoms = Set.empty
    }

runtime :: WithTarget => String -> Label
runtime name =
  let (ns, ':':fun) = break (== ':') name
   in mkLabel ("_" ++ ns ++ "__" ++ fun)

uuid :: M Int
uuid = #uuid <<+= 1

label :: WithTarget => M Label
label = do uuid <- uuid; pure (mkLabel ("_lbl." ++ show uuid))

alloc :: M Operand
alloc = do
  allocated <- use #allocated
  let (index, new) = head (filter (\(_, o) -> o `Set.notMember` allocated) operands)
  #allocated %= Set.insert new
  #required_slots %= max index
  tell [ movq UNBOUND_TAG new ]
  pure new
  where
    operands = [(i, MemReg (WORD_SIZE * (-i)) vstack) | i <- [1..]]

free :: Operand -> M ()
free op = do
  tell [ movq UNBOUND_TAG op ]
  #allocated %= Set.delete op

withAlloc :: (Operand -> M ()) -> M ()
withAlloc body = do
  x <- alloc
  body x
  free x

atom :: WithTarget => Atom -> M Operand
atom a = do
  #atoms %= Set.insert a
  pure (Static (mkAtomLabel a))

variable :: Var -> M Operand
variable var = do
  variables <- use #variables
  case variables Map.!? var of
    Just op -> pure op
    Nothing -> error ("unknown variable: " ++ var.getString)

argument :: Int -> M Operand
argument ~arg = do
  ~base <- use #stackframe
  pure (MemReg (base + arg * WORD_SIZE) vstack)

data MatchOp = ByValue Operand | ByRef Operand

pat :: WithTarget => Operand -> Pat -> M () -> M ()
pat value p on_match_fail = case p of
  PatVar var -> do
    var <- variable var
    match "match:variable" [ByValue value, ByRef var] []

  PatWildcard ->
    pure ()

  PatInteger i -> do
    match "match:integer" [ByValue value, ByValue (fromInteger i)] []
  
  PatAtom a -> do
    a <- atom a
    match "match:atom" [ByValue value, ByValue a] []

  PatTuple ps -> do
    match "match:tuple" [ByValue value, ByValue (fromIntegral (length ps))] ps

  PatNil ->
    match "match:nil" [ByValue value] []

  PatCons p1 p2 ->
    match "match:cons" [ByValue value] [p1, p2]

  PatMatch p1 p2 -> do
    pat value p1 on_match_fail
    pat value p2 on_match_fail

  where
    match fun ops children | length ops > 2 = error "to many args for match"
    match fun ops children = do
      done <- label
      for_ (zip ops [rdi, rsi]) \case
        (ByValue op, t) -> tell [ movq op t ]
        (ByRef   op, t) -> tell [ leaq op t ]
      tell [ callq  (runtime fun) ]
      tell [ cmpq   0 rax ]
      tell [ jne    done ]
      on_match_fail
      tell [ _label done]
      
      -- match children
      unless (null children) do
        withAlloc \values -> do
          tell [ movq rax values ]
          ifor_ children \i p -> do
            withAlloc \child -> do
              tell [ movq values rax ]
              tell [ movq (MemReg (i * WORD_SIZE) rax) rax ]
              tell [ movq rax child ]
              pat child p on_match_fail
      

expr :: WithTarget => Operand -> Expr -> M ()
expr result = \case
  Integer i -> do
    tell [ movq  (fromInteger i) rdi ]
    tell [ callq (runtime "alloc:integer") ]
    tell [ movq  rax result ]
  
  Atom a -> do
    a <- atom a
    tell [ movq a rax ]
    tell [ movq rax result ]
  
  Tuple es -> do
    let size = length es

    values <- for es \e -> do
      value <- alloc
      value <~ e
      pure value
    
    tell [ movq  (fromIntegral size) rdi ]
    tell [ callq (runtime "alloc:tuple") ]
    tell [ movq  rax result ]

    when (odd size) do
      tell [ subq WORD_SIZE rsp ]
    for_ (reverse values) \value -> do
      tell [ pushq value ]

    tell [ movq  rax rdi ]
    tell [ movq  (fromIntegral size) rsi ]
    tell [ movq  rsp rdx ]
    tell [ callq (runtime "fill:tuple") ]

    let restore = WORD_SIZE * if odd size then size + 1 else size
    tell [ addq (Imm (fromIntegral restore)) rsp ]

    for_ values free
  
  Nil{} ->
    tell [ movq NIL_TAG result ]
  
  Cons a b -> do
    a' <- alloc; a' <~ a
    b' <- alloc; b' <~ b
  
    tell [ callq (runtime "alloc:cons") ]
    tell [ movq  rax result ]

    tell [ movq  rax rdi ]
    tell [ movq  a' rsi ]
    tell [ movq  b' rdx ]
    tell [ callq (runtime "fill:cons") ]

    free a'
    free b'

  Arg arg -> do
    arg <- argument arg
    tell [ movq arg rax ]
    tell [ movq rax result ]
  
  Var var -> do
    var <- variable var
    tell [ movq var rax ]
    tell [ movq rax result ]
  
  Fun funid -> do
    tell [ movq (Static (mkFunctionLabel funid)) rax ]
    tell [ movq rax result ]
  
  BinOp op a b -> do
    withAlloc \temp -> do
      result <~ a
      temp   <~ b
      tell [ movq  result rdi ]
      tell [ movq  temp rsi ]
      tell [ callq (runtime ("binop:" ++ binOpName op)) ]
      tell [ movq  rax result ]
  
  UnOp op a -> do
    result <~ a
    tell [ movq  result rdi ]
    tell [ callq (runtime ("unop:" ++ unOpName op)) ]
    tell [ movq  rax result ]
  
  Match p e -> do
    result <~ e
    pat result p do
      tell [ movq result rdi ]
      tell [ callq (runtime "throw:badmatch") ]
  
  Apply f es -> do
    ifor_ es \i e -> MemReg (i * WORD_SIZE) vstack <~ e
    tell [ addq  (Imm (length es * WORD_SIZE)) vstack ]
    tell [ callq (mkFunctionLabel f) ]
    tell [ movq  rax result ]
  
  TailApply f es -> do
    values <- for es \e -> do
      value <- alloc
      value <~ e
      pure value
    ifor_ values \i value -> do
      arg <- argument i
      tell [ movq value rax ]
      tell [ movq rax arg ]
    for_ values free
    tell [ jmp (mkFunctionBodyLabel f) ]
  
  DynApply f es ->
    withAlloc \f' -> do
      f' <~ f
      tell [ movq   f' rdi ]
      tell [ movq   (fromIntegral (length es)) rsi ]
      tell [ callq  (runtime "assert:fun") ]

      ifor_ es \i e -> MemReg (i * WORD_SIZE) vstack <~ e
      tell [ movq   f' rax ]
      tell [ addq   (Imm (length es * WORD_SIZE)) vstack ]
      tell [ callq' rax ]
      tell [ movq   rax result ]
  
  Receive cases -> do
    loop <- label; done <- label

    withAlloc \msg -> do
      tell [ _label loop ]
      tell [ callq  (runtime "receive:pick") ]
      tell [ movq   rax msg ]

      for_ cases \(CaseBranch p body) -> do
        fail  <- label
        saved <- saveVars p
        pat msg p do
          tell [ jmp fail ]
        result <~ body
        tell [ jmp done ]
        tell [ _label fail ]
        restoreVars p saved

      tell [ jmp    loop ]
      tell [ _label done ]
      tell [ callq  (runtime "receive:success") ]
  
  Case e cases -> do
    done <- label

    result <~ e
    for_ cases \(CaseBranch p body) -> do
      fail  <- label
      saved <- saveVars p
      pat result p do
        tell [ jmp fail ]
      result <~ body
      tell [ jmp    done ]
      tell [ _label fail ]
      restoreVars p saved
    
    tell [ movq  result rdi ]
    tell [ callq (runtime "throw:case_clause") ]

    tell [ _label done]

  Seq a b -> do
    result <~ a
    result <~ b

  where
    (<~) = expr

    saveVars pat = do
      for (Set.toList (allVars pat)) \var -> do
        var   <- variable var
        saved <- alloc
        tell [ movq var rax ]
        tell [ movq rax saved ]
        pure saved

    restoreVars pat saved = do
      for_ (zip (Set.toList (allVars pat)) saved) \(var, saved) -> do
        var <- variable var
        tell [ movq saved rax ]
        tell [ movq rax var ]
        free saved


funWrapper :: WithTarget => FunId -> M () -> M ()
funWrapper funid body = mdo
  tell [ _newline ]
  tell [ _globl (mkFunctionLabel funid) ]
  tell [ _align WORD_SIZE ]
  tell [ _skip  FUN_TAG ]
  let size = mkFunctionEndLabel funid ++ " - " ++ mkFunctionLabel funid
  tell [ _label (mkFunctionMetaOptLabel funid "size"), _quad size ]

  #allocated .= Set.empty
  #required_slots .= 0
  #stackframe .= stackframe
  
  tell [ _label (mkFunctionLabel funid) ]

  body
  tell [ _label (mkFunctionEndLabel funid) ]

  requiredSlots <- use #required_slots
  let stackframe = -(requiredSlots + funid.arity) * WORD_SIZE

  tell [ _label (mkFunctionMetaLabel funid) ]
  tell [ _label (mkFunctionMetaOptLabel funid "arity"),  _quad  (show funid.arity) ]
  tell [ _label (mkFunctionMetaOptLabel funid "name"),   _asciz (show (prettyFunIdNoArity funid)) ]
  tell [ _label (mkFunctionMetaOptLabel funid "name_a"), _asciz (show (prettyFunId funid)) ]

  allocated <- use #allocated
  unless (Set.null allocated) do
    error "local heap is not empty"

decl :: WithTarget => Decl -> M ()

decl Native{agner = funid, c} = funWrapper funid do
  tell [ subq WORD_SIZE rsp ]

  tell [ movq  (Static (mkFunctionMetaOptLabel funid "name_a")) rdi ]
  tell [ callq (runtime "runtime:yield") ]
  
  let args = [0..funid.arity - 1]
  let argsOnStack = max 0 (funid.arity - length fast_args)

  for_ (zip fast_args args) \(reg, arg) -> do
    arg <- argument arg
    tell [ movq arg reg ]

  when (odd argsOnStack) do
    tell [ subq WORD_SIZE rsp ]

  tell [ movq (Static (mkFunctionMetaLabel funid)) rdi ]
  for_ (drop (length fast_args) args) \arg -> do
    arg <- argument arg
    tell [ pushq arg ]

  tell [ callq (mkLabel c) ]

  when (argsOnStack > 0) do
    let toRestore = argsOnStack + if odd argsOnStack then 1 else 0
    tell [ addq (Imm (fromIntegral toRestore * WORD_SIZE)) rsp ]

  tell [ subq (Imm (funid.arity * WORD_SIZE)) vstack ]
  tell [ addq WORD_SIZE rsp ]
  tell [ retq ]
  where
    fast_args = [rsi, rdx, rcx, r8, r9]

decl FunDecl{funid, body} = funWrapper funid mdo
  tell [ _comment "prologue" ]
  tell [ subq WORD_SIZE rsp ]
  tell [ addq  (Imm (requiredSlots * WORD_SIZE)) vstack ]
  tell [ callq (runtime "runtime:save_vstack") ]

  tell [ _label (mkFunctionBodyLabel funid) ]
  tell [ movq  (Static (mkFunctionMetaOptLabel funid "name_a")) rdi ]
  tell [ callq (runtime "runtime:yield") ]

  tell [ _comment "initializing locals" ]
  #variables <~ Map.unions <$> for (Set.toList (allVars body)) \var -> do
    slot <- alloc
    tell [ movq UNBOUND_TAG slot ]
    pure (Map.singleton var slot)

  tell [ _comment "body" ]
  withAlloc \result -> do
    expr result body
    tell [ movq result rbx ]

  for_ (allVars body) \var -> do
    free =<< variable var
    
  tell [ _comment "epilogue" ]
  requiredSlots <- use #required_slots
  tell [ subq  (Imm ((requiredSlots + funid.arity) * WORD_SIZE)) vstack ]
  tell [ callq (runtime "runtime:save_vstack") ]
  tell [ addq WORD_SIZE rsp ]
  tell [ movq rbx rax ]
  tell [ retq ]


entryPoint :: WithTarget => M ()
entryPoint = do
  tell [ _newline ]
  tell [ _globl (mkLabel "main") ]
  tell [ _label (mkLabel "main") ]
  tell [ subq   WORD_SIZE rsp ]

  share_atom "true"
  share_atom "false"
  share_atom "ok"
  share_atom "infinity"

  tell [ movq   (Static (mkFunctionLabel "root:main/0")) rdi ]
  tell [ callq  (runtime "runtime:start") ]

  tell [ addq   WORD_SIZE rsp ]
  tell [ movq   0 rax ]
  tell [ retq ]

  where
    share_atom :: Atom -> M ()
    share_atom a = do
      a' <- atom a
      tell [ movq a' rdi ]
      tell [ callq (mkLabel ("share_atom_" ++ a.getString)) ]

atoms :: WithTarget => M ()
atoms = do
  tell [ _newline ]
  tell [ _comment "atoms" ]
  atoms <- use #atoms
  for_ atoms \atom -> do
    tell [ _newline ]
    tell [ _align WORD_SIZE ]
    tell [ _skip  ATOM_TAG ]
    tell [ _label (mkAtomLabel atom), _asciz (show atom) ]

compile :: Target -> Module -> Prog
compile target mod = let ?target = target in runM do
  tell [ _newline ]
  tell [ _text ]
  entryPoint
  for_ mod.decls decl

  tell [ _newline ]
  tell [ _data ]
  atoms


-- utils

mkLabel :: WithTarget => String -> Label
mkLabel name = case ?target of
  Linux -> name
  MacOS -> "_" ++ name

mkFunctionLabel, mkFunctionBodyLabel, mkFunctionEndLabel, mkFunctionMetaLabel :: WithTarget => FunId -> Label
mkFunctionMetaOptLabel :: WithTarget => FunId -> String -> Label
mkFunctionLabel (MkFunId ns f arity) = mkLabel (ns.getString ++ "." ++ f.getString ++ "." ++ show arity)
mkFunctionBodyLabel funid = mkFunctionLabel funid ++ ".body" 
mkFunctionEndLabel funid = mkFunctionLabel funid ++ ".end"
mkFunctionMetaLabel funid = mkFunctionLabel funid ++ ".meta"
mkFunctionMetaOptLabel funid field = mkFunctionMetaLabel funid ++ "." ++ field

mkAtomLabel :: WithTarget => Atom -> Label
mkAtomLabel atom = mkLabel ("_atom." ++ atom.getString)

vstack :: FromReg a => a
vstack = fromReg R12
