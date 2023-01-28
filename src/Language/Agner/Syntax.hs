module Language.Agner.Syntax where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.List.NonEmpty (NonEmpty)
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

type Var = String
type Atom = String
type FunName = String

data FunId = (:/) { name :: FunName, arity :: Int }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

data BinOp
  = (:+)
  | (:-)
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data Expr
  = Integer Integer
  | Atom Atom
  | BinOp Expr BinOp Expr
  | Var Var
  | Match Pat Expr
  | Apply Atom [Expr]
  deriving stock (Show)

data Pat
  = PatVar Var
  | PatWildcard
  | PatInteger Integer
  | PatAtom Atom
  deriving (Show)

type Exprs = NonEmpty Expr

data FunClause = MkFunClause
  { funid :: FunId
  , pats :: [Pat]
  , body :: Exprs
  }

data FunDecl = MkFunDecl
  { funid :: FunId
  , clauses :: [FunClause]
  }

data Module = MkModule
  { decls :: [FunDecl]
  }

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
  Apply _ es -> foldMap exprVars es

exprsVars :: Exprs -> Set Var
exprsVars = foldMap exprVars

funClauseVars :: FunClause -> Set Var
funClauseVars clause =
  foldMap patVars clause.pats `Set.union` exprsVars clause.body

funDeclVars :: FunDecl -> Set Var
funDeclVars decl =
  foldMap funClauseVars decl.clauses

moduleVars :: Module -> Set Var
moduleVars mod = foldMap funDeclVars mod.decls

