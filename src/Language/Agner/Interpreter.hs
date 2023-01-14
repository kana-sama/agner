module Language.Agner.Interpreter (eval, denoteBinOp) where

import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Value (Value)
import Language.Agner.Value qualified as Value

denoteBinOp :: Syntax.BinOp -> (Value -> Value -> Value)
denoteBinOp = \case
  (Syntax.:+) -> \(Value.Integer a) (Value.Integer b) -> Value.Integer (a + b)

eval :: Syntax.Expr -> Value
eval = \case
  Syntax.Integer i -> Value.Integer i
  Syntax.BinOp a op b -> (denoteBinOp op) (eval a) (eval b)