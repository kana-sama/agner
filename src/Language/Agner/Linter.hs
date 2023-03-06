module Language.Agner.Linter (Error(..), prettyError, check) where

import Data.Set qualified as Set

import Control.Monad.Except

import Language.Agner.Prelude
import Language.Agner.Syntax
import Language.Agner.Denote (bifs)
import Language.Agner.Pretty qualified as Pretty

data Error
  = UnboundVariable Var
  | UndefinedFunction FunId

prettyError :: Error -> String
prettyError = \case
  UnboundVariable var ->
    "variable '" ++ var ++ "A' is unbound"
  UndefinedFunction funid ->
    "function " ++ Pretty.funid'' funid ++ " undefined"

check :: Module -> Maybe Error
check module_ = (toMaybe . runExcept) do
  checkVariables module_
  checkFunctions module_
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

checkFunctions :: Module -> Except Error ()
checkFunctions module_ = flip evalStateT Set.empty do
  put bifs
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
      BinOp e1 _ e2 -> expr e1 *> expr e2
      Apply _ f es -> do
        check f
        for_ es expr
      DynApply e es -> for_ (e:es) expr
      Var v -> pure ()
      Match p e -> expr e

    check f = do
      funs <- get
      when (f `Set.notMember` funs) do
        throwError (UndefinedFunction f)

