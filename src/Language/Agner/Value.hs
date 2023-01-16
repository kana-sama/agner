module Language.Agner.Value (Value(..), same, encode) where

data Value
  = Integer Integer
  deriving stock (Show)

same :: Value -> Value -> Bool
same (Integer i) (Integer j) | i == j = True
same _ _ = False

encode :: Value -> String
encode = \case
  Integer i -> show i