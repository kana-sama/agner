module Language.Agner.SM (Prog, Instr(..), compile, run) where

import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Value (Value)
import Language.Agner.Value qualified as Value
import Language.Agner.Interpreter (denoteBinOp)

data Instr
  = PUSHI Integer
  | BINOP Syntax.BinOp

type Prog = [Instr]
type Stack = [Value]

compile :: Syntax.Expr -> Prog
compile = \case
  Syntax.Integer i ->
    [PUSHI i]
  Syntax.BinOp a op b ->
    compile a ++ compile b ++ [BINOP op]

runInstr :: Instr -> Stack -> Stack
runInstr (PUSHI i) stack = Value.Integer i : stack
runInstr (BINOP op) (b : a : stack) = (denoteBinOp op) a b : stack

run :: Prog -> Value
run prog =
  let [result] = foldl (flip runInstr) [] prog
   in result