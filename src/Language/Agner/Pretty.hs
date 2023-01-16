module Language.Agner.Pretty where

import Language.Agner.Syntax

parens :: String -> String
parens s = "(" ++ s ++ ")"

(<+>) :: String -> String -> String
(<+>) a b = a ++ " " ++ b

binOp :: BinOp -> String
binOp = \case
  (:+) -> "+"

expr :: Expr -> String
expr = \case
  Integer i -> show i
  BinOp a op b -> parens (expr a <+> binOp op <+> expr b)