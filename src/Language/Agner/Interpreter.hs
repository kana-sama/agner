module Language.Agner.Interpreter (eval) where

import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Value (Value)
import Language.Agner.Value qualified as Value

eval :: Syntax.Expr -> Value
eval = \case
  Syntax.Integer i -> Value.Integer i