module Language.Agner.Syntax where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.List.NonEmpty (NonEmpty)

type Var = String
type Atom = String

data BinOp
  = (:+)
  deriving stock (Show)

data Expr
  = Integer Integer
  | Atom Atom
  | BinOp Expr BinOp Expr
  | Var Var
  | Match Pat Expr
  deriving stock (Show)

data Pat
  = PatVar Var
  | PatWildcard
  | PatInteger Integer
  | PatAtom Atom
  deriving (Show)

type Exprs = NonEmpty Expr

type Module = Exprs

patVars :: Pat -> Set Var
patVars = \case
  PatVar var -> Set.singleton var
  PatWildcard -> Set.empty
  PatInteger _ -> Set.empty
  PatAtom _ -> Set.empty

exprVars :: Expr -> Set Var
exprVars = \case
  Integer _ -> Set.empty
  Atom _ -> Set.empty
  BinOp a _ b -> exprVars a `Set.union` exprVars b
  Var v -> Set.singleton v
  Match p e -> patVars p `Set.union` exprVars e

exprsVars :: Exprs -> Set Var
exprsVars = foldMap exprVars

moduleVars :: Module -> Set Var
moduleVars = exprsVars

