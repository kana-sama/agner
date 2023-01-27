module Language.Agner.Value (Value(..), same, encode) where

import Language.Agner.Syntax qualified as Syntax

data Value
  = Integer Integer
  | Atom Syntax.Atom
  deriving stock (Show)

encode :: Value -> String
encode = \case
  Integer i -> show i
  Atom a -> a

same :: Value -> Value -> Bool
same (Integer i) (Integer j) | i == j = True
same (Atom a) (Atom b) | a == b = True
same _ _ = False
