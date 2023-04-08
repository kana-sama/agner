module Language.Agner.SM where

import Language.Agner.Prelude

import Data.Set qualified as Set
import Data.Map.Strict qualified as Map
import Data.List qualified as List
import Data.Aeson (ToJSON)
import Data.Typeable (eqT)
import Data.IORef (IORef, newIORef, modifyIORef')

import Language.Agner.Syntax (FunId)
import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Value (Value, PID)
import Language.Agner.Value qualified as Value
import Language.Agner.Denote qualified as Denote
import Language.Agner.Syntax (BinOp)
import Language.Agner.BiF qualified as BiF


data OnMatchFail = Throw | NextClause FunId (Maybe Int) | JumpTo Label
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

type Label = String

data Instr
  = PUSH_I Integer
  | PUSH_ATOM Syntax.Atom
  | PUSH_FUN FunId
  | PUSH_TUPLE Int
  | PUSH_NIL
  | PUSH_CONS
  | BINOP Syntax.BinOp
  | UNOP Syntax.UnOp
  | DROP
  | DUP
  | CALL{funid :: FunId, tailness :: Syntax.CallTailness}
  | DYN_CALL{arity :: Int}
  
  | FUNCTION{funid :: FunId, vars :: [Syntax.Var]}
  | YIELD{funid :: FunId}
  | CLAUSE{funid :: FunId, clauseIndex :: Int, vars :: [Syntax.Var]}
  | FAIL_CLAUSE{funid :: FunId, clauseIndex :: Int}
  | FUNCTION_END{funid :: FunId}
  | LEAVE FunId
  | RET

  | GOTO Label
  | LABEL Label

  | RECEIVE_PICK
  | RECEIVE_PICKED
  | RECEIVE_SUCCESS

  | SAVE [Syntax.Var]
  | RESTORE [Syntax.Var]

  | LOAD Syntax.Var
  | MATCH_I Integer OnMatchFail
  | MATCH_VAR Syntax.Var OnMatchFail
  | MATCH_ATOM Syntax.Atom OnMatchFail
  | MATCH_TUPLE Int OnMatchFail
  | MATCH_NIL OnMatchFail
  | MATCH_CONS OnMatchFail

  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

type Prog = [Instr]

type M = StateT Int (Writer Prog)

label :: String -> M Label
label template = do
  uid <- state \s -> (s, s + 1)
  pure (template ++ "." ++ show uid)

compileExpr :: Syntax.Expr -> M ()
compileExpr = \case
  Syntax.Integer i -> do
    tell [PUSH_I i]
  Syntax.Atom a -> do
    tell [PUSH_ATOM a]
  Syntax.Fun f -> do
    tell [PUSH_FUN f]
  Syntax.Tuple es -> do
    for_ es compileExpr
    tell [PUSH_TUPLE (length es)]
  Syntax.Nil -> do
    tell [PUSH_NIL]
  Syntax.Cons a b -> do
    compileExpr a
    compileExpr b
    tell [PUSH_CONS]
  Syntax.BinOp a op b -> do
    compileExpr a
    compileExpr b
    tell [BINOP op]
  Syntax.UnOp op a -> do
    compileExpr a
    tell [UNOP op]
  Syntax.Var v -> do
    tell [LOAD v]
  Syntax.Match p e -> do
    compileExpr e
    tell [DUP]
    compilePat Throw p
  Syntax.Apply tailness f args -> do
    for_ args compileExpr
    tell [CALL f tailness]
  Syntax.DynApply f args -> do
    compileExpr f
    for_ args compileExpr
    tell [DYN_CALL (length args)]
  Syntax.Send a b -> do
    compileExpr a
    compileExpr b
    tell [CALL "erlang:send/2" Syntax.SimpleCall]
  Syntax.Receive cases -> mdo
    peek <- label "receive_peek"; tell [LABEL peek]
    tell [RECEIVE_PICK]
    for_ (zipWithLast cases) \((p, es), last) -> mdo
      let vars = Set.toList (Syntax.patVars p)
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

compilePat :: OnMatchFail -> Syntax.Pat -> M ()
compilePat onMatchFail = \case
  Syntax.PatVar var -> do
    tell [MATCH_VAR var onMatchFail]
  Syntax.PatInteger i -> do
    tell [MATCH_I i onMatchFail]
  Syntax.PatWildcard -> do
    tell [DROP]
  Syntax.PatAtom a -> do
    tell [MATCH_ATOM a onMatchFail]
  Syntax.PatTuple ps -> do
    tell [MATCH_TUPLE (length ps) onMatchFail]
    for_ ps (compilePat onMatchFail)
  Syntax.PatNil -> do
    tell [MATCH_NIL onMatchFail]
  Syntax.PatCons a b -> do
    tell [MATCH_CONS onMatchFail]
    compilePat onMatchFail a
    compilePat onMatchFail b

zipWithLast :: [a] -> [(a, Bool)]
zipWithLast xs = reverse (zip (reverse xs) ([True] ++ repeat False))

compileExprs :: Syntax.Exprs -> M ()
compileExprs exprs = do
  for_ (zipWithLast exprs) \(e, last) -> do
    compileExpr e
    unless last do tell [DROP]

compileFunDecl :: Syntax.FunDecl -> M ()
compileFunDecl funDecl = do
  tell [FUNCTION funDecl.funid (Set.toList (Syntax.funDeclVars funDecl))]
  tell [YIELD funDecl.funid]
  for_ (zipWithLast (zip funDecl.clauses [0..])) \((clause, ix), last) -> do
    compileFunClause clause ix last
  tell [FAIL_CLAUSE funDecl.funid (length funDecl.clauses)]
  tell [FUNCTION_END funDecl.funid]

-- -- args are in original order on stack
compileFunClause :: Syntax.FunClause -> Int -> Bool -> M ()
compileFunClause clause ix isLast = do
  tell [CLAUSE clause.funid ix (Set.toList (Syntax.funClauseVars clause))]
  let onFail = NextClause clause.funid (if isLast then Nothing else Just (ix + 1))
  for_ clause.pats \pat -> do
    compilePat onFail pat
  for_ clause.guards \guard -> do
    compileExpr guard
    tell [MATCH_ATOM "true" onFail]
  compileExprs clause.body
  tell [LEAVE clause.funid]
  tell [RET]

compileModule :: Syntax.Module -> Prog
compileModule mod = (execWriter . flip evalStateT 0) do
  for_ mod.decls \decl -> do
    compileFunDecl decl
