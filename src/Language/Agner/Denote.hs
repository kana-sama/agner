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
  | UndefinedFunction Syntax.FunId
  | NoMatch Syntax.Pat Value Env
  | BinOp_BadArgs Syntax.BinOp
  | NoEntryPoint
  | NoFunctionClauseMatching Syntax.FunId [Value]
  deriving stock (Show)
  deriving anyclass (Exception)

binOp :: Syntax.BinOp -> (Value -> Value -> Value)
binOp = \case
  (Syntax.:+) -> \a b ->
    case (a, b) of
      (Value.Integer a, Value.Integer b) -> Value.Integer (a + b)
      _ -> throw (BinOp_BadArgs (Syntax.:+))
  (Syntax.:-) -> \a b ->
    case (a, b) of
      (Value.Integer a, Value.Integer b) -> Value.Integer (a - b)
      _ -> throw (BinOp_BadArgs (Syntax.:-))

type Env = Map Syntax.Var Value
type FunEnv = Map Syntax.FunId ([Value] -> Value)

funDecl :: FunEnv -> Syntax.FunDecl -> ([Value] -> Value)
funDecl funs decl = let ?funs = funs in goClauses decl.clauses
  where
    goClauses [] =
      \values -> throw (NoFunctionClauseMatching decl.funid values)
    goClauses (c:cs) =
      \values ->
        case matchClause c.pats values Map.empty of
          Nothing -> goClauses cs values
          Just env -> let (v, _) = (exprs c.body) env in v

    matchClause :: [Syntax.Pat] -> [Value] -> Env -> Maybe Env
    matchClause [] [] env = Just env
    matchClause (p:ps) (v:vs) env =
      case match p v env of
        Just env -> matchClause ps vs env
        Nothing -> Nothing
    matchClause _ _ _ = error "Denote.tryClause: impossible!!"

expr :: (?funs :: FunEnv) => Syntax.Expr -> (Env -> (Value, Env))
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
  Syntax.Apply f args -> runState do
    f <- case ?funs Map.!? (f Syntax.:/ length args) of
           Nothing -> throw (UndefinedFunction (f Syntax.:/ length args))
           Just f -> pure f
    vals <- traverse (state . expr) args
    pure (f vals)

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
      
exprs :: (?funs :: FunEnv) => Syntax.Exprs -> (Env -> (Value, Env))
exprs (e :| es) = runState do
  v <- state (expr e)
  foldlM (\_ e -> state (expr e)) v es

module_ :: Syntax.Module -> Value
module_ mod =
  let funs = Map.fromList [(d.funid, funDecl funs d) | d <- mod.decls] 
   in case funs Map.!? ("init" Syntax.:/ 0) of
        Nothing -> throw NoEntryPoint
        Just init -> init []
