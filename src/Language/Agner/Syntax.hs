module Language.Agner.Syntax where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.List.NonEmpty (NonEmpty)

data BinOp
  = (:+)
  deriving stock (Show)

type Var = String

data Expr
  = Integer Integer
  | BinOp Expr BinOp Expr
  | Var Var
  | Match Pat Expr
  deriving stock (Show)

data Pat
  = PatVar Var
  | PatInteger Integer
  | PatWildcard
  deriving stock (Show)

type Exprs = NonEmpty Expr

type Module = Exprs

patVars :: Pat -> Set String
patVars = \case
  PatVar v -> Set.singleton v
  PatInteger _ -> Set.empty
  PatWildcard -> Set.empty

exprVars :: Expr -> Set String
exprVars = \case
  Integer _ -> Set.empty
  BinOp a _ b -> exprVars a <> exprVars b
  Var v -> Set.singleton v
  Match p e -> patVars p <> exprVars e

exprsVars :: Module -> Set String
exprsVars = foldMap exprVars

moduleVars :: Module -> Set String
moduleVars = exprsVars