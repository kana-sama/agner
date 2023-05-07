# include "../../../runtime/tags.h"

module Language.Agner.X64
  ( Target(..)
  , EntryPointCtx
  , compileModule
  , compileEntryPoint
  ) where

import Paths_agner (getDataFileName)
import System.IO.Unsafe (unsafePerformIO)

import Data.X64
import Data.Set qualified as Set
import Data.Map.Strict qualified as Map

import Language.Agner.Prelude
import Language.Agner.Syntax

data Target = Linux | MacOS
type WithTarget = ?target :: Target

data AnonFun = MkAnonFun{funid :: FunId, env :: [Var], clauses :: [Clause]}

data Ctx = MkCtx
  { uuid :: Int
  , allocated :: Set Operand
  , required_slots :: Int
  , stackframe :: ~Int
  , variables :: Map Var Operand
  , atoms :: Set Atom
  , anon_funs :: [AnonFun]
  } deriving stock (Generic)

type M = StateT Ctx (Writer Prog)

defaultStackFrame :: Int
defaultStackFrame = error "required_slots is not evaluated yet"

shouldBeDesugared :: Show a => a -> b
shouldBeDesugared a = error ("should be desugared: " ++ show a)

emptyCtx :: Ctx
emptyCtx = MkCtx
  { uuid = 0
  , allocated = Set.empty
  , required_slots = 0
  , stackframe = defaultStackFrame
  , variables = Map.empty
  , atoms = Set.empty
  , anon_funs = []
  }

runM :: Ctx -> M () -> (Ctx, Prog)
runM ctx m = runWriter (execStateT m ctx)

runtime :: WithTarget => String -> Label
runtime name =
  let (ns, ':':fun) = break (== ':') name
   in mkLabel ("_" ++ ns ++ "__" ++ fun)

uuid :: M Int
uuid = #uuid <<+= 1

label :: WithTarget => M Label
label = do uuid <- uuid; pure (mkLabel ("_lbl." ++ show uuid))

closureFunId :: WithTarget => Int -> M FunId
closureFunId arity = do
  uuid <- uuid
  pure MkFunId{ns = "_anon", name = coerce ("n" ++ show uuid), arity}

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

  p@PatRecord{} -> shouldBeDesugared p

  where
    match fun ops children | length ops > 4 = error "to many args for match"
    match fun ops children = do
      done <- label
      for_ (zip ops [rdi, rsi, rdx, rcx]) \case
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


guardExpr :: WithTarget => Operand -> GuardExpr -> M ()
guardExpr = coerce expr


guardSeq :: WithTarget => GuardSeq -> Label -> M ()
guardSeq or_guards fail = do
  guards_done <- label
  true <- atom "true"
  for_ or_guards \and_guards -> do
    next_or_guard <- label
    for_ and_guards \guard -> do
      withAlloc \value -> do
        guardExpr value guard
        tell [ movq value rax ]
        tell [ cmpq rax true ]
        tell [ jne next_or_guard ]
    tell [ jmp    guards_done ]
    tell [ _label next_or_guard ]
  tell [ jmp    fail ]
  tell [ _label guards_done ]


caseBranch :: WithTarget => Operand -> Operand -> M () -> Label -> CaseBranch -> M ()
caseBranch value result afterMatch done branch = do
  fail  <- label
  saved <- saveVars branch.pat
  pat value branch.pat do
    tell [ jmp fail ]

  unless (null branch.guards) do
    guardSeq branch.guards fail

  afterMatch

  exprs result branch.body
  tell [ jmp    done ]

  tell [ _label fail ]
  restoreVars branch.pat saved
  where
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

    when (odd size) do
      tell [ subq WORD_SIZE rsp ]
    for_ (reverse values) \value -> do
      tell [ pushq value ]

    tell [ movq  (fromIntegral size) rdi ]
    tell [ movq  rsp rsi ]
    tell [ callq (runtime "alloc:tuple") ]
    tell [ movq  rax result ]

    let restore = WORD_SIZE * if odd size then size + 1 else size
    tell [ addq (Imm (fromIntegral restore)) rsp ]

    for_ values free

  Nil ->
    tell [ movq NIL_TAG result ]

  Cons a b -> do
    withAlloc \a_ -> withAlloc \b_ -> do
      a_ <~ a; b_ <~ b
      tell [ movq  a_ rdi ]
      tell [ movq  b_ rsi ]
      tell [ callq (runtime "alloc:cons") ]
      tell [ movq  rax result ]

  Var var -> do
    var_op   <- variable var
    var_atom <- atom (MkAtom var.getString)
    tell [ movq var_op rdi ]
    tell [ movq var_atom rsi ]
    tell [ callq (runtime "assert:bound") ]
    tell [ movq rdi result ]

  Fun{funid} -> do
    tell [ movq (Static (mkFunctionLabel funid)) rax ]
    tell [ movq rax result ]

  FunL{clauses} -> do
    let env = Set.toList (allVars clauses)
    let size = length env

    funid <- closureFunId clauses.head.pats.length
    #anon_funs %= (MkAnonFun{funid, env, clauses}:)

    when (odd size) do
      tell [ subq WORD_SIZE rsp ]
    for_ (reverse env) \var -> do
      var_op <- variable var
      tell [ pushq var_op ]

    tell [ leaq  (MemRegL (mkFunctionLabel funid) rip) rdi ]
    tell [ movq  (fromIntegral size) rsi ]
    tell [ movq  rsp rdx ]
    tell [ callq (runtime "alloc:closure") ]
    tell [ movq  rax result ]

    let restore = WORD_SIZE * if odd size then size + 1 else size
    tell [ addq (Imm (fromIntegral restore)) rsp ]

  Match p e -> do
    result <~ e
    pat result p do
      tell [ movq result rdi ]
      tell [ callq (runtime "throw:badmatch") ]

  Apply f es -> do
    apply result f [ApplyExpr e | e <- es]

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
      call_lbl <- label
      f' <~ f

      tell [ movq   f' rdi ]
      tell [ movq   (fromIntegral (length es)) rsi ]
      tell [ callq  (runtime "assert:fun") ]
      tell [ cmpq   (ImmL "FUN_KIND_CLOSURE") rax ]
      tell [ jne    call_lbl ]

      tell [ movq   f' rdi ]
      tell [ callq  (runtime "closure:get_env") ]
      tell [ movq   rax closureEnv ]

      tell [ movq   f' rdi ]
      tell [ callq  (runtime "closure:get_fun") ]
      tell [ movq   rax f' ]

      tell [ _label call_lbl ]
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
      for_ cases \branch -> do
        let afterMatch = tell [ callq (runtime "receive:success") ]
        caseBranch msg result afterMatch done branch
      tell [ jmp    loop ]
      tell [ _label done ]

  Case e cases -> do
    done <- label

    withAlloc \value -> do
      value <~ e
      for_ cases (caseBranch value result (pure ()) done)

      tell [ movq  value rdi ]
      tell [ callq (runtime "throw:case_clause") ]

      tell [ _label done]

  Begin es -> exprs result es

  Catch e -> do
    done <- label; on_exception <- label

    tell [ leaq  (MemRegL on_exception rip) rdi]
    tell [ callq (runtime "runtime:catch") ]

    result <~ e
    tell [ jmp done]

    tell [ _label on_exception ]
    tell [ movq rdi result ]

    tell [ _label done ]

  e@MapUpdate{}      -> shouldBeDesugared e
  e@ListComp{}       -> shouldBeDesugared e
  e@MapComp{}        -> shouldBeDesugared e
  e@AndAlso{}        -> shouldBeDesugared e
  e@OrElse{}         -> shouldBeDesugared e
  e@BinOp{}          -> shouldBeDesugared e
  e@Send{}           -> shouldBeDesugared e
  e@Map{}            -> shouldBeDesugared e
  e@If{}             -> shouldBeDesugared e
  e@UnOp{}           -> shouldBeDesugared e
  e@Maybe{}          -> shouldBeDesugared e
  e@Record{}         -> shouldBeDesugared e
  e@RecordGet{}      -> shouldBeDesugared e
  e@RecordUpdate{}   -> shouldBeDesugared e
  e@RecordSelector{} -> shouldBeDesugared e

  where
    (<~) = expr

exprs :: WithTarget => Operand -> Exprs -> M ()
exprs result es = for_ es (expr result)


data ApplyArg = ApplyExpr Expr | ApplyOp Operand

apply :: WithTarget => Operand -> FunId -> [ApplyArg] -> M ()
apply result f args = do
  args_ops <- for args \case
    ApplyExpr arg -> do
      value <- alloc
      expr value arg
      pure value
    ApplyOp op -> pure op

  ifor_ args_ops \i op -> do
    let arg = MemReg (i * WORD_SIZE) vstack
    tell [ movq op rax ]
    tell [ movq rax arg ]
  tell [ addq  (Imm (length args * WORD_SIZE)) vstack ]
  tell [ callq (mkFunctionLabel f) ]
  tell [ movq  rax result ]

  for_ (zip args_ops args) \case
    (op, ApplyExpr{}) -> free op
    (_, ApplyOp{})    -> pure ()

funWrapper :: WithTarget => FunId -> M () -> M ()
funWrapper funid body = mdo
  tell [ _newline ]
  tell [ _text ]
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

clause :: WithTarget => FunId -> Clause -> Operand -> Label -> M ()
clause funid clause result fail = do
  tell [ _comment "function clause" ]
  clauseLabel <- mkFunctionClauseLabel funid
  tell [ _label clauseLabel ]

  tell [ _comment "unbound pats vars" ]
  for_ (allVars clause.pats) \var -> do
    var <- variable var
    tell [ movq UNBOUND_TAG var ]

  tell [ _comment "match clause patterns" ]
  ifor_ clause.pats \arg p -> do
    arg <- argument arg
    pat arg p do tell [ jmp fail ]

  tell [ _comment "check clause guards" ]
  unless (null clause.guards) do
    guardSeq clause.guards fail

  tell [ _comment "clause body" ]
  exprs result clause.body

function :: WithTarget => FunId -> [Clause] -> (M (Map Var Operand)) -> M ()
function funid clauses init_scope = funWrapper funid mdo
  tell [ _comment "prologue" ]
  tell [ subq WORD_SIZE rsp ]
  tell [ addq  (Imm (requiredSlots * WORD_SIZE)) vstack ]
  tell [ callq (runtime "runtime:save_vstack") ]

  tell [ _label (mkFunctionBodyLabel funid) ]
  tell [ leaq  (MemRegL (mkFunctionMetaOptLabel funid "name_a") rip) rdi ]
  tell [ callq (runtime "runtime:yield") ]

  tell [ _comment "initializing locals" ]
  #variables <~ init_scope

  tell [ _comment "function clauses" ]
  withAlloc \result -> do
    for_ clauses \c -> do
      next_clause <- label
      clause funid c result next_clause
      tell [ movq   result rax ]
      tell [ jmp    epilogue ]
      tell [ _label next_clause ]

  tell [ _newline ]
  tell [ _comment "failure clause" ]
  fail_clause <- label
  tell [ _label fail_clause ]
  tell [ leaq   (MemRegL (mkFunctionMetaLabel funid) rip) rdi ]
  tell [ leaq   args rsi ]; args <- argument 0
  tell [ callq  (runtime "throw:function_clause") ]

  for_ (allVars clauses) \var -> do
    free =<< variable var

  epilogue <- label
  tell [ _comment "epilogue" ]
  tell [ _label epilogue ]
  requiredSlots <- use #required_slots
  tell [ movq  rax rbx ]
  tell [ subq  (Imm ((requiredSlots + funid.arity) * WORD_SIZE)) vstack ]
  tell [ callq (runtime "runtime:save_vstack") ]
  tell [ addq WORD_SIZE rsp ]
  tell [ movq rbx rax ]
  tell [ retq ]


decl :: WithTarget => Decl -> M ()

decl Primitive{funid} = funWrapper funid do
  tell [ subq WORD_SIZE rsp ]

  tell [ leaq  (MemRegL (mkFunctionMetaOptLabel funid "name_a") rip) rdi ]
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

  tell [ callq (mkLabel (prim_name funid)) ]

  when (argsOnStack > 0) do
    let toRestore = argsOnStack + if odd argsOnStack then 1 else 0
    tell [ addq (Imm (fromIntegral toRestore * WORD_SIZE)) rsp ]

  tell [ subq (Imm (funid.arity * WORD_SIZE)) vstack ]
  tell [ addq WORD_SIZE rsp ]
  tell [ retq ]
  where
    fast_args = [rsi, rdx, rcx, r8, r9]

    prim_name funid =
      "_" ++ funid.ns.getString ++ "__" ++ funid.name.getString ++ "__" ++ show funid.arity

decl FunDecl{funid, clauses} = function funid clauses do
  Map.unions <$> for (Set.toList (allVars clauses)) \var -> do
    slot <- alloc
    tell [ _comment ("variable " ++ var.getString) ]
    tell [ movq UNBOUND_TAG slot ]
    pure (Map.singleton var slot)

decl ExportDecl{names} = do
  for_ names \name -> do
    tell [ _globl (mkFunctionLabel name) ]

decl d@ImportDecl{} = shouldBeDesugared d
decl d@RecordDecl{} = shouldBeDesugared d

anonFun :: WithTarget => AnonFun -> M ()
anonFun anon = function anon.funid anon.clauses do
  Map.unions <$> ifor anon.env \i var -> do
    slot <- alloc
    tell [ _comment ("variable " ++ var.getString) ]
    tell [ movq (MemReg (i * WORD_SIZE) closureEnv) rax ]
    tell [ movq rax slot ]
    pure (Map.singleton var slot)


entryPoint :: WithTarget => M ()
entryPoint = do
  tell [ _newline ]
  tell [ _text ]
  tell [ _globl (mkLabel "main") ]
  tell [ _label (mkLabel "main") ]
  tell [ subq   WORD_SIZE rsp ]

  share_atom "true"
  share_atom "false"
  share_atom "ok"
  share_atom "infinity"

  tell [ movq   (Static (mkFunctionLabel "main:main/0")) rdi ]
  tell [ callq  (runtime "runtime:start") ]

  tell [ addq   WORD_SIZE rsp ]
  tell [ movq   0 rax ]
  tell [ retq ]

  tell [ _newline ]
  tell [ _comment "atoms" ]
  tell [ _data ]
  atoms <- use #atoms
  for_ atoms \atom -> do
    tell [ _newline ]
    tell [ _align WORD_SIZE ]
    tell [ _skip  ATOM_TAG ]
    tell [ _globl (mkAtomLabel atom) ]
    tell [ _label (mkAtomLabel atom), _asciz (show atom) ]

  where
    share_atom :: Atom -> M ()
    share_atom a = do
      a' <- atom a
      tell [ movq a' rdi ]
      tell [ callq (mkLabel ("share_" ++ a.getString)) ]


module_ :: WithTarget => Module -> M ()
module_ module_ = do
  tell [ _include do unsafePerformIO (getDataFileName "runtime/tags.h") ]

  tell [ _newline ]
  tell [ _comment ("module: " ++ module_.name.getString) ]
  tell [ _newline ]
  for_ module_.decls decl

  tell [ _newline ]
  tell [ _comment "anon functions"]
  tell [ _newline ]
  whileM (not . null <$> use #anon_funs) do
    anon <- #anon_funs %%= \(l:ls) -> (l, ls)
    anonFun anon


data EntryPointCtx = MkEntryPointCtx {atoms :: Set Atom}
instance Semigroup EntryPointCtx where
  a <> b = MkEntryPointCtx{atoms = a.atoms <> b.atoms}
instance Monoid EntryPointCtx where
  mempty = MkEntryPointCtx{atoms = mempty}


compileModule :: Target -> Module -> (EntryPointCtx, String)
compileModule target m = do
  let ?target = target
  let (ctx, prog) = runM emptyCtx (module_ m)
  (toEntryPointCtx ctx, prettyProg prog)
  where
    toEntryPointCtx MkCtx{atoms} = MkEntryPointCtx{atoms}

compileEntryPoint :: Target -> EntryPointCtx -> String
compileEntryPoint target ctx = do
  let ?target = target
  let (_, prog) = runM (fromEntryPointCtx ctx) entryPoint
  prettyProg prog
  where
    fromEntryPointCtx MkEntryPointCtx{atoms} =
      emptyCtx{atoms} :: Ctx

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

mkFunctionClauseLabel :: WithTarget => FunId -> M Label
mkFunctionClauseLabel funid = do
  uuid <- uuid
  pure (mkFunctionLabel funid ++ ".clause" ++ show uuid)

mkAtomLabel :: WithTarget => Atom -> Label
mkAtomLabel atom = mkLabel ("_atom." ++ atom.getString)

vstack, closureEnv :: FromReg a => a
vstack     = fromReg R12
closureEnv = fromReg R13
