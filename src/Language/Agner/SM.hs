module Language.Agner.SM (Prog, Instr(..), compile, run) where

import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Value (Value)
import Language.Agner.Value qualified as Value

data Instr
  = PUSHI Integer

type Prog = [Instr]

compile :: Syntax.Expr -> Prog
compile = \case
  Syntax.Integer i -> [PUSHI i]

run :: Prog -> Value
run = go []
  where
    go stack (PUSHI i : is) = go (Value.Integer i : stack) is
    go (v : _) [] = v