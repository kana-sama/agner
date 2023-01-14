module Language.Agner.Value (Value(..), encode) where

data Value
  = Integer Integer
  deriving stock (Show)

encode :: Value -> String
encode = \case
  Integer i -> show i