module Language.Agner.SM where

import Language.Agner.Prelude
import Language.Agner.Syntax
import Data.Set qualified as Set

data OnMatchFail = Throw | NextClause FunId (Maybe Int) | JumpTo Label
  deriving stock (Show)

type Label = String

data Instr
  = PUSH_I Integer
  | PUSH_ATOM Atom
  | PUSH_FUN FunId
  | PUSH_TUPLE Int
  | PUSH_NIL
  | PUSH_CONS
  | BINOP BinOp
  | UNOP UnOp
  | DROP
  | DUP
  | CALL{funid :: FunId, tailness :: CallTailness}
  | DYN_CALL{arity :: Int}
  
  | FUNCTION{funid :: FunId, vars :: [Var]}
  | YIELD{funid :: FunId}
  | CLAUSE{funid :: FunId, clauseIndex :: Int, vars :: [Var]}
  | FAIL_CLAUSE{funid :: FunId, clauseIndex :: Int}
  | FUNCTION_END{funid :: FunId}
  | LEAVE FunId
  | RET

  | GOTO Label
  | LABEL Label

  | RECEIVE_PICK
  | RECEIVE_PICKED
  | RECEIVE_SUCCESS

  | SAVE [Var]
  | RESTORE [Var]

  | LOAD Var
  | MATCH_I Integer OnMatchFail
  | MATCH_VAR Var OnMatchFail
  | MATCH_ATOM Atom OnMatchFail
  | MATCH_TUPLE Int OnMatchFail
  | MATCH_NIL OnMatchFail
  | MATCH_CONS OnMatchFail

  | SHORT_CIRCUIT Bool Label

  deriving stock (Show)

type Prog = [Instr]

type M = StateT Int (Writer Prog)

label :: String -> M Label
label template = do
  uid <- state \s -> (s, s + 1)
  pure (template ++ "." ++ show uid)

compileExpr :: Expr -> M ()
compileExpr = \case
  Integer i -> do
    tell [PUSH_I i]
  Atom a -> do
    tell [PUSH_ATOM a]
  Fun f -> do
    tell [PUSH_FUN f]
  Tuple es -> do
    for_ es compileExpr
    tell [PUSH_TUPLE (length es)]
  Nil -> do
    tell [PUSH_NIL]
  Cons a b -> do
    compileExpr a
    compileExpr b
    tell [PUSH_CONS]
  BinOp op a b -> do
    compileExpr a
    compileExpr b
    tell [BINOP op]
  UnOp op a -> do
    compileExpr a
    tell [UNOP op]
  Var v -> do
    tell [LOAD v]
  Match p e -> do
    compileExpr e
    tell [DUP]
    compilePat Throw p
  Apply tailness f args -> do
    for_ args compileExpr
    tell [CALL f tailness]
  DynApply f args -> do
    compileExpr f
    for_ args compileExpr
    tell [DYN_CALL (length args)]
  Send a b -> do
    compileExpr a
    compileExpr b
    tell [CALL "erlang:send/2" SimpleCall]
  Receive cases -> mdo
    peek <- label "receive_peek"; tell [LABEL peek]
    tell [RECEIVE_PICK]
    for_ (zipWithLast cases) \((p, es), last) -> mdo
      let vars = Set.toList (patVars p)
      tell [SAVE vars]

      tell [RECEIVE_PICKED]
      compilePat (JumpTo case_failure) p

      tell [RECEIVE_SUCCESS]
      compileExprs es
      tell [GOTO success]

      -- for_ should generate code, that will produce exactly one value on stack
      -- chain of `compileExprs es` produces `length es` values on stack
      when (not last) do
        tell [DROP]

      case_failure <- label "receive_case_failure"; tell [LABEL case_failure]
      tell [RESTORE vars]
    tell [GOTO peek]
    success <- label "receive_success"; tell [LABEL success]
  AndAlso a b -> shortCircuit True a b
  OrElse  a b -> shortCircuit False a b
  Begin es -> compileExprs es

shortCircuit :: Bool -> Expr -> Expr -> M ()
shortCircuit t a b = mdo
  compileExpr a
  tell [SHORT_CIRCUIT t finally]
  compileExpr b
  tell [GOTO finally]
  tell [DROP] -- remove result of b from stack statically
  finally <- label "SHORT_CIRCUIT_finally"; tell [LABEL finally]

compilePat :: OnMatchFail -> Pat -> M ()
compilePat onMatchFail = \case
  PatVar var -> do
    tell [MATCH_VAR var onMatchFail]
  PatInteger i -> do
    tell [MATCH_I i onMatchFail]
  PatWildcard -> do
    tell [DROP]
  PatAtom a -> do
    tell [MATCH_ATOM a onMatchFail]
  PatTuple ps -> do
    tell [MATCH_TUPLE (length ps) onMatchFail]
    for_ ps (compilePat onMatchFail)
  PatNil -> do
    tell [MATCH_NIL onMatchFail]
  PatCons a b -> do
    tell [MATCH_CONS onMatchFail]
    compilePat onMatchFail a
    compilePat onMatchFail b

zipWithLast :: [a] -> [(a, Bool)]
zipWithLast xs = reverse (zip (reverse xs) ([True] ++ repeat False))

compileExprs :: Exprs -> M ()
compileExprs exprs = do
  for_ (zipWithLast exprs) \(e, last) -> do
    compileExpr e
    unless last do tell [DROP]

compileFunDecl :: FunDecl -> M ()
compileFunDecl funDecl = do
  tell [FUNCTION funDecl.funid (Set.toList (funDeclVars funDecl))]
  tell [YIELD funDecl.funid]
  for_ (zipWithLast (zip funDecl.clauses [0..])) \((clause, ix), last) -> do
    compileFunClause clause ix last
  tell [FAIL_CLAUSE funDecl.funid (length funDecl.clauses)]
  tell [FUNCTION_END funDecl.funid]

-- -- args are in original order on stack
compileFunClause :: FunClause -> Int -> Bool -> M ()
compileFunClause clause ix isLast = do
  tell [CLAUSE clause.funid ix (Set.toList (funClauseVars clause))]
  let onFail = NextClause clause.funid (if isLast then Nothing else Just (ix + 1))
  for_ clause.pats \pat -> do
    compilePat onFail pat
  for_ clause.guards \guard -> do
    compileExpr guard
    tell [MATCH_ATOM "true" onFail]
  compileExprs clause.body
  tell [LEAVE clause.funid]
  tell [RET]

compileModule :: Module -> Prog
compileModule mod = (execWriter . flip evalStateT 0) do
  for_ mod.decls \decl -> do
    compileFunDecl decl
