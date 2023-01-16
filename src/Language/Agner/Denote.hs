module Language.Agner.Denote (Ex(..), binOp, expr, exprs, module_) where

import Data.List.NonEmpty qualified as NonEmpty

import Control.Exception (Exception, throw)

import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Value (Value)
import Language.Agner.Value qualified as Value

data Ex
  deriving stock (Show)
  deriving anyclass (Exception)

binOp :: Syntax.BinOp -> (Value -> Value -> Value)
binOp = \case
  (Syntax.:+) -> \a b ->
    case (a, b) of
      (Value.Integer a, Value.Integer b) -> Value.Integer (a + b)

expr :: Syntax.Expr -> Value
expr = \case
  Syntax.Integer i -> Value.Integer i
  Syntax.BinOp a op b -> (binOp op) (expr a) (expr b)

exprs :: Syntax.Exprs -> Value
exprs es = foldr1 seq (expr <$> es)

module_ :: Syntax.Module -> Value
module_ = exprs