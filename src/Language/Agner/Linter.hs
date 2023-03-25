module Language.Agner.Linter (Error(..), prettyError, check) where

import Data.Set qualified as Set

import Language.Agner.Prelude
import Language.Agner.Syntax
import Language.Agner.BiF qualified as BiF
import Language.Agner.Prettier qualified as Prettier

data Error
  = UnboundVariable Var
  | UndefinedFunction FunId
  | FunctionAlreadyDefined FunId

prettyError :: Error -> String
prettyError = \case
  UnboundVariable var ->
    "variable '" ++ var ++ "A' is unbound"
  UndefinedFunction funid ->
    "function " ++ Prettier.string Prettier.funId funid ++
      "/" ++ show funid.arity ++ " undefined"
  FunctionAlreadyDefined funid ->
    "function " ++ Prettier.string Prettier.funId funid ++
      "/" ++ show funid.arity ++ " already defined"

check :: Module -> Maybe Error
check module_ = (toMaybe . runExcept) do
  checkVariables module_
  checkFunctions module_
  duplicateDefinitions module_
  where
    toMaybe (Left e) = Just e
    toMaybe (Right ()) = Nothing

checkVariables :: Module -> Except Error ()
checkVariables module_ = do
  for_ module_.decls \decl -> do
    for_ decl.clauses \clause -> do
      flip evalStateT (foldMap patVars clause.pats) do
        for_ clause.body expr
  where
    expr = \case
      Integer _ -> pure ()
      Atom _ -> pure ()
      Fun _ -> pure ()
      Tuple es -> for_ es expr
      Nil -> pure ()
      Cons a b -> do expr a; expr b
      BinOp e1 _ e2 -> expr e1 *> expr e2
      Apply _ _ es -> for_ es expr
      DynApply e es -> for_ (e:es) expr
      Var v -> do
        vars <- get
        when (v `Set.notMember` vars) do
          throwError (UnboundVariable v)
      Match p e -> do
        modify (Set.union (patVars p))
        expr e
      Send e1 e2 -> expr e1 *> expr e2
      Receive cases -> for_ cases \(p, es) -> do
        state <- get
        modify (Set.union (patVars p))
        for_ es expr
        put state

checkFunctions :: Module -> Except Error ()
checkFunctions module_ = flip evalStateT Set.empty do
  for_ module_.decls \decl -> do
    modify (Set.insert decl.funid)
  for_ module_.decls \decl ->
    for_ decl.clauses \clause -> do
      for_ clause.body expr
  where
    expr = \case
      Integer _ -> pure ()
      Atom _ -> pure ()
      Fun f -> do
        funs <- get
        check f
      Tuple es -> for_ es expr
      Nil -> pure ()
      Cons a b -> do expr a; expr b
      BinOp e1 _ e2 -> expr e1 *> expr e2
      Apply _ f es -> do
        check f
        for_ es expr
      DynApply e es -> for_ (e:es) expr
      Var v -> pure ()
      Match p e -> expr e
      Send e1 e2 -> expr e1 *> expr e2
      Receive cases -> (traverse_ . traverse_ . traverse_) expr cases

    check f | Just _ <- BiF.parse f = pure ()
    check f = do
      funs <- get
      when (f `Set.notMember` funs) do
        throwError (UndefinedFunction f)

duplicateDefinitions :: Module -> Except Error ()
duplicateDefinitions module_ = go Set.empty [d.funid | d <- module_.decls]
  where
    go _ [] = pure ()
    go funs (f:fs)
      | f `Set.member` funs = throwError (FunctionAlreadyDefined f)
      | otherwise = go (Set.insert f funs) fs
