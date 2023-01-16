module Language.Agner.Syntax where

import Data.List.NonEmpty (NonEmpty)

data BinOp
  = (:+)
  deriving stock (Show)

data Expr
  = Integer Integer
  | BinOp Expr BinOp Expr
  deriving stock (Show)

type Exprs = NonEmpty Expr

type Module = Exprs