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
    let (ns,    src1) = parseNamespace src0
        (name,  src2) = parseName src1
        (arity,   "") = parseArity src2
      in MkFunId ns name arity
    where
      parseNamespace src
        | ':' `elem` src =
            let (ns, ':':src') = List.break (== ':') src
             in (Just ns, src')
        | otherwise = (Nothing, src)
      parseName src =
        tail <$> List.break (== '/') src
      parseArity src =
        (read src, "")

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
  | (:++)
  | (:>=)
  | (:=<)
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data CallTailness = SimpleCall | TailCall
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data Expr
  = Integer Integer
  | Atom Atom
  | Fun FunId
  | Tuple [Expr]
  | Nil
  | Cons Expr Expr
  | BinOp Expr BinOp Expr
  | Var Var
  | Match Pat Expr
  | Apply CallTailness FunId [Expr]
  | DynApply Expr [Expr]
  | Send Expr Expr
  | Receive [(Pat, Exprs)]
  deriving stock (Show)

viewList :: Expr -> Maybe [Expr]
viewList = \case
  Nil -> Just []
  Cons a b -> do xs <- viewList b; pure (a:xs)
  _ -> Nothing

pattern List :: [Expr] -> Expr
pattern List es <- (viewList -> Just es)
  where
    List es = foldr Cons Nil es

data Pat
  = PatVar Var
  | PatWildcard
  | PatInteger Integer
  | PatAtom Atom
  | PatTuple [Pat]
  | PatNil
  | PatCons Pat Pat
  deriving (Show)

viewPatList :: Pat -> Maybe [Pat]
viewPatList = \case
  PatNil -> Just []
  PatCons a b -> do xs <- viewPatList b; pure (a:xs)
  _ -> Nothing

pattern PatList :: [Pat] -> Pat
pattern PatList ps <- (viewPatList -> Just ps)
  where
    PatList ps = foldr PatCons PatNil ps

type Exprs = [Expr]

data FunClause = MkFunClause
  { funid :: FunId
  , pats :: [Pat]
  , guards :: [Expr]
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
  PatTuple ps -> foldMap patVars ps
  PatNil -> Set.empty
  PatCons a b -> patVars a `Set.union` patVars b

exprVars :: Expr -> Set Var
exprVars = \case
  Integer _ -> Set.empty
  Atom _ -> Set.empty
  Fun _ -> Set.empty
  Tuple es -> foldMap exprVars es
  Nil -> Set.empty
  Cons a b -> exprVars a `Set.union` exprVars b
  BinOp a _ b -> exprVars a `Set.union` exprVars b
  Var v -> Set.singleton v
  Match p e -> patVars p `Set.union` exprVars e
  Apply _ _ es -> foldMap exprVars es
  DynApply _ es -> foldMap exprVars es
  Send a b -> exprVars a `Set.union` exprVars b
  Receive cases -> Set.unions [patVars p `Set.union` exprsVars e | (p, e) <- cases]

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
