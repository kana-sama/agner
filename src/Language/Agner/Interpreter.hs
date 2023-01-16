module Language.Agner.Interpreter (Ex(..), eval, denoteBinOp) where

import Control.Exception (Exception, throw)

import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Value (Value)
import Language.Agner.Value qualified as Value

data Ex
  deriving stock (Show)
  deriving anyclass (Exception)

denoteBinOp :: Syntax.BinOp -> (Value -> Value -> Value)
denoteBinOp = \case
  (Syntax.:+) -> \a b ->
    case (a, b) of
      (Value.Integer a, Value.Integer b) -> Value.Integer (a + b)

eval :: Syntax.Expr -> Value
eval = \case
  Syntax.Integer i -> Value.Integer i
  Syntax.BinOp a op b -> (denoteBinOp op) (eval a) (eval b)