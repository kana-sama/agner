module Language.Agner.Syntax where

data BinOp
  = (:+)
  deriving stock (Show)

data Expr
  = Integer Integer
  | BinOp Expr BinOp Expr
  deriving stock (Show)
