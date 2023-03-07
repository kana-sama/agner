module Language.Agner.Value (Value(..), same, encode) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Data.List (intercalate)

import Language.Agner.Syntax qualified as Syntax

data Value
  = Integer Integer
  | Atom Syntax.Atom
  | Fun Syntax.FunId
  | Tuple [Value]
  deriving stock (Show, Generic, Eq)
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
  Tuple vs ->
    "{" ++ intercalate "," [encode v | v <- vs] ++ "}"

same :: Value -> Value -> Bool
same = (==)
