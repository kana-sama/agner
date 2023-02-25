module Language.Agner.Pretty where

import Data.List.NonEmpty (NonEmpty)
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

sepBy1 :: (a -> String) -> String -> (NonEmpty a -> String)
sepBy1 p s =
  \xs -> concat (NonEmpty.intersperse s (p <$> xs))

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

expr :: Expr -> String
expr = \case
  Integer i -> show i
  Atom a -> a
  BinOp a op b -> parens (expr a <+> binOp op <+> expr b)
  Var v -> v
  Match p e -> parens (pat p <+> "=" <+> expr e)
  Apply (MkFunId ns f _a) es ->
    concat
      [ case ns of Just ns -> ns ++ ":"; Nothing -> ""
      , f
      , parens ((expr `sepBy` ", ") es)
      ]

exprs :: Exprs -> String
exprs = expr `sepBy1` ", "

funClause :: FunClause -> String
funClause clause =
  clause.funid.name ++ parens ((pat `sepBy` ", ") clause.pats) <+> "->" <+> exprs clause.body

funDecl :: FunDecl -> String
funDecl decl =
  intercalate ";\n" [funClause c | c <- decl.clauses] ++ ".\n"

module_ :: Module -> String
module_ mod =
  unlines [funDecl d | d <- mod.decls]