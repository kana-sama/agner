module Language.Agner.Pretty where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.List qualified as List

import Language.Agner.Syntax

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

pat :: Pat -> String
pat = \case
  PatVar v -> v
  PatInteger i -> show i
  PatWildcard -> "_"

expr :: Expr -> String
expr = \case
  Integer i -> show i
  BinOp a op b -> parens (expr a <+> binOp op <+> expr b)
  Var v -> v
  Match p e -> parens (pat p <+> "=" <+> expr e)

exprs :: Exprs -> String
exprs = expr `sepBy1` ", "

module_ :: Module -> String
module_ = exprs