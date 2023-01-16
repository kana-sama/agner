module Language.Agner.SM (Ex(..), Prog, Instr(..), compile, run) where

import Control.Exception (Exception, throw)

import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Value (Value)
import Language.Agner.Value qualified as Value
import Language.Agner.Interpreter (denoteBinOp)

data Ex
  = NotEnoughValuesForBinOp Syntax.BinOp Stack
  | NotEnoughValuesForResult
  deriving stock (Show)
  deriving anyclass (Exception)

data Instr
  = PUSH_I Integer
  | BINOP Syntax.BinOp

type Prog = [Instr]
type Stack = [Value]

compile :: Syntax.Expr -> Prog
compile = \case
  Syntax.Integer i ->
    [PUSH_I i]
  Syntax.BinOp a op b ->
    compile a ++ compile b ++ [BINOP op]

runInstr :: Instr -> Stack -> Stack
runInstr (PUSH_I i) stack = Value.Integer i : stack
runInstr (BINOP op) (b : a : stack) = (denoteBinOp op) a b : stack
runInstr (BINOP op) stack = throw (NotEnoughValuesForBinOp op stack)

run :: Prog -> Value
run prog =
  case foldl (flip runInstr) [] prog of
    result:_ -> result
    _ -> throw NotEnoughValuesForResult