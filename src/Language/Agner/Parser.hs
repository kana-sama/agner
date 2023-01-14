module Language.Agner.Parser where

import Language.Agner.Syntax qualified as Syntax

parse :: String -> Syntax.Expr
parse = Syntax.Integer . read