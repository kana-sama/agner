module Language.Agner.Pretty where

import Data.List.NonEmpty qualified as NonEmpty
import Data.List qualified as List

import Language.Agner.Syntax
import Data.List (intercalate)

parens :: String -> String
parens s = "(" ++ s ++ ")"

(<+>) :: String -> String -> String
(<+>) a b = a ++ " " ++ b

sepBy :: (a -> String) -> String -> ([a] -> String)
sepBy p s =
  \xs -> concat (List.intersperse s (p <$> xs))

binOp :: BinOp -> String
binOp = \case
  (:+) -> "+"
  (:-) -> "-"

pat :: Pat -> String
pat = \case
  PatVar v -> v
  PatInteger i -> show i
  PatWildcard -> "_"
  PatAtom a -> a

funid' :: FunId -> String
funid' (MkFunId ns f _a) =
  (case ns of Just ns -> ns ++ ":"; Nothing -> "") ++ f

funid'' :: FunId -> String
funid'' f = funid' f ++ "/" ++ show f.arity

expr :: Expr -> String
expr = \case
  Integer i -> show i
  Atom a -> a
  BinOp a op b -> parens (expr a <+> binOp op <+> expr b)
  Var v -> v
  Match p e -> parens (pat p <+> "=" <+> expr e)
  Apply _ f es ->
    concat
      [ funid' f
      , parens ((expr `sepBy` ", ") es)
      ]
  DynApply e1 es -> parens (expr e1) ++ parens ((expr `sepBy` ", ") es)
  Fun f -> "fun" <+> funid' f ++ "/" ++ show f.arity

exprs :: Exprs -> String
exprs = expr `sepBy` ", "

funClause :: FunClause -> String
funClause clause =
  clause.funid.name ++ parens ((pat `sepBy` ", ") clause.pats) <+> "->" <+> exprs clause.body

funDecl :: FunDecl -> String
funDecl decl =
  intercalate ";\n" [funClause c | c <- decl.clauses] ++ ".\n"

module_ :: Module -> String
module_ mod =
  unlines [funDecl d | d <- mod.decls]