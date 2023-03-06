module Language.Agner.Value (Value(..), same, encode) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

import Language.Agner.Syntax qualified as Syntax

data Value
  = Integer Integer
  | Atom Syntax.Atom
  | Fun Syntax.FunId
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

encode :: Value -> String
encode = \case
  Integer i -> show i
  Atom a -> a
  Fun f ->
    concat
      [ "fun "
      , case f.ns of Nothing -> ""; Just ns -> ns ++ ":"
      , f.name
      , "/"
      , show f.arity
      ]

same :: Value -> Value -> Bool
same (Integer i) (Integer j) | i == j = True
same (Atom a) (Atom b) | a == b = True
same (Fun a) (Fun b) | a == b = True
same _ _ = False
