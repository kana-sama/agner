module Language.Agner.Value (Value(..), PID(..), pattern List, same, encode, isPrintableLatin1) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Data.List (intercalate)
import Data.Char qualified as Char

import Language.Agner.Syntax qualified as Syntax

newtype PID = MkPID{unPID :: Integer}
  deriving stock (Show, Eq)
  deriving newtype (ToJSON)

data Value
  = Integer Integer
  | Atom Syntax.Atom
  | Fun Syntax.FunId
  | Tuple [Value]
  | Nil
  | Cons Value Value
  | PID PID
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON)

viewList :: Value -> Maybe [Value]
viewList = \case
  Nil -> Just []
  Cons a b -> do xs <- viewList b; pure (a:xs)
  _ -> Nothing

pattern List :: [Value] -> Value
pattern List es <- (viewList -> Just es)
  where
    List es = foldr Cons Nil es

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
  Tuple vs -> "{" ++ intercalate "," [encode v | v <- vs] ++ "}"
  List vs | all isPrintableLatin1 vs -> show [Char.chr (fromInteger i) | Integer i <- vs]
  List vs -> "[" ++ intercalate "," [encode v | v <- vs] ++ "]"
  Nil -> "[]"
  Cons a b -> "[" ++ encode a ++ "|" ++ encode b ++ "]"
  PID (MkPID x) -> "<" ++ show x ++ ">"

isPrintableLatin1 :: Value -> Bool
isPrintableLatin1 (Integer i) = or
  [ i >= 032 && i <= 126
  , i >= 160 && i  < 255
  , c `elem` ("\n\r\t\v\b\f" ++ [Char.chr 27])
  ] where c = Char.chr (fromInteger i)
isPrintableLatin1 _ = False

same :: Value -> Value -> Bool
same = (==)
