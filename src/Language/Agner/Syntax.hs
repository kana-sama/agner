module Language.Agner.Syntax where

import Language.Agner.Prelude

import Data.Set qualified as Set
import Data.List.NonEmpty (NonEmpty)
import Data.Aeson (ToJSON)
import Data.List qualified as List

type Var = String
type Atom = String
type FunName = String
type ModuleName = String

pattern (:/) :: FunName -> Int -> FunId
pattern f :/ arity = MkFunId Nothing f arity

data FunId = MkFunId { ns :: Maybe ModuleName, name :: FunName, arity :: Int }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (ToJSON)

instance IsString FunId where
  fromString src0 = 
    let (ns, ':':src1) = List.break (== ':') src0
        (f, '/':src2) = List.break (== '/') src1
        a = read src2
      in MkFunId (Just ns) f a

instance Show FunId where
  show f =
    (show . concat)
      [ case f.ns of Nothing -> ""; Just ns -> ns ++ ":"
      , f.name
      , "/"
      , show f.arity
      ]

data BinOp
  = (:+)
  | (:-)
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data CallTailness = SimpleCall | TailCall
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data Expr
  = Integer Integer
  | Atom Atom
  | BinOp Expr BinOp Expr
  | Var Var
  | Match Pat Expr
  | Apply CallTailness FunId [Expr]
  -- | DynApply Expr [Expr]
  -- | Fun FunId
  deriving stock (Show)

data Pat
  = PatVar Var
  | PatWildcard
  | PatInteger Integer
  | PatAtom Atom
  deriving (Show)

type Exprs = [Expr]

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
  Apply _ _ es -> foldMap exprVars es
  -- DynApply _ es -> foldMap exprVars es
  -- Fun _ -> Set.empty

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

