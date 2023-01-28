module Language.Agner.Denote (Ex(..), binOp, expr, exprs, module_) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Foldable (foldlM)

import Control.Exception (Exception, throw)
import Control.Monad.State.Strict

import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Value (Value)
import Language.Agner.Value qualified as Value

data Ex
  = UnboundVariable Syntax.Var Env
  | NoMatch Syntax.Pat Value Env
  | BinOp_BadArgs Syntax.BinOp
  deriving stock (Show)
  deriving anyclass (Exception)

binOp :: Syntax.BinOp -> (Value -> Value -> Value)
binOp = \case
  (Syntax.:+) -> \a b ->
    case (a, b) of
      (Value.Integer a, Value.Integer b) -> Value.Integer (a + b)
      _ -> throw (BinOp_BadArgs (Syntax.:+))

type Env = Map Syntax.Var Value

expr :: Syntax.Expr -> (Env -> (Value, Env))
expr = \case
  Syntax.Integer i -> runState do
    pure (Value.Integer i)
  Syntax.Atom a -> runState do
    pure (Value.Atom a)
  Syntax.BinOp a op b -> runState do
    a <- state (expr a)
    b <- state (expr b)
    pure ((binOp op) a b)
  Syntax.Var var -> runState do
    env <- get
    case env Map.!? var of
      Just val -> pure val
      Nothing -> throw (UnboundVariable var env)
  Syntax.Match p e -> runState do
    v <- state (expr e)
    env <- get
    case (match p v) env of
      Nothing -> throw (NoMatch p v env)
      Just env -> put env *> pure v

match :: Syntax.Pat -> Value -> (Env -> Maybe Env)
match Syntax.PatWildcard _ =
  \env -> Just env
match (Syntax.PatInteger i) value
  | Value.same (Value.Integer i) value = \env -> Just env
  | otherwise = \env -> Nothing
match (Syntax.PatAtom a) value
  | Value.same (Value.Atom a) value = \env -> Just env
  | otherwise = \env -> Nothing
match (Syntax.PatVar var) val =
  \env ->
    case env Map.!? var of
      Nothing -> Just (Map.insert var val env)
      Just val'
        | Value.same val val' -> Just env
        | otherwise -> Nothing
  
      
exprs :: Syntax.Exprs -> (Env -> (Value, Env))
exprs (e :| es) = runState do
  v <- state (expr e)
  foldlM (\_ e -> state (expr e)) v es

module_ :: Syntax.Module -> Value
module_ es = evalState (state (exprs es)) Map.empty