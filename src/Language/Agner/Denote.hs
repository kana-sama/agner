module Language.Agner.Denote where

import Language.Agner.Prelude

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Value (Value)
import Language.Agner.Value qualified as Value

data Ex
  = UnboundVariable Syntax.Var Env
  | UndefinedFunction Syntax.FunId
  | UnknownBiF Syntax.FunId
  | NoMatch Syntax.Pat Value Env
  | BinOp_BadArgs Syntax.BinOp
  | NoEntryPoint
  | NoFunctionClauseMatching Syntax.FunId [Value]
  | BadFunction Value
  | BadArity Syntax.FunId Int
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
type FunEnv = Map Syntax.FunId ([Value] -> IO Value)

funDecl :: FunEnv -> Syntax.FunDecl -> ([Value] -> IO Value)
funDecl funs decl = goClauses decl.clauses
  where
    goClauses [] =
      \values -> throw (NoFunctionClauseMatching decl.funid values)
    goClauses (c:cs) =
      \values ->
        case matchClause c.pats values Map.empty of
          Nothing -> goClauses cs values
          Just env -> do
            (v, _) <- (let ?funs = funs in exprs c.body) env
            pure v

    matchClause :: [Syntax.Pat] -> [Value] -> Env -> Maybe Env
    matchClause [] [] env = Just env
    matchClause (p:ps) (v:vs) env =
      case match p v env of
        Just env -> matchClause ps vs env
        Nothing -> Nothing
    matchClause _ _ _ = error "Denote.tryClause: impossible!!"

expr :: (?funs :: FunEnv) => Syntax.Expr -> (Env -> IO (Value, Env))
expr = \case
  Syntax.Integer i -> runStateT do
    pure (Value.Integer i)
  Syntax.Atom a -> runStateT do
    pure (Value.Atom a)
  Syntax.Fun f -> runStateT do
    pure (Value.Fun f)
  Syntax.BinOp a op b -> runStateT do
    a <- StateT (expr a)
    b <- StateT (expr b)
    pure ((binOp op) a b)
  Syntax.Var var -> runStateT do
    env <- get
    case env Map.!? var of
      Just val -> pure val
      Nothing -> throw (UnboundVariable var env)
  Syntax.Match p e -> runStateT do
    v <- StateT (expr e)
    env <- get
    case (match p v) env of
      Nothing -> throw (NoMatch p v env)
      Just env -> put env *> pure v
  Syntax.Apply _ funid args ->
    apply funid args
  Syntax.DynApply e args -> runStateT do
    StateT (expr e) >>= \case
      Value.Fun funid -> StateT (apply funid args)
      value -> throw (BadFunction value)

apply :: (?funs :: FunEnv) => Syntax.FunId -> [Syntax.Expr] -> (Env -> IO (Value, Env))
apply funid args = runStateT do
  when (funid.arity /= length args) do
    throw (BadArity funid (length args))
  let !fun = resolveFunction funid
  vals <- traverse (StateT . expr) args
  liftIO (fun vals)

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

exprs :: (?funs :: FunEnv) => Syntax.Exprs -> (Env -> IO (Value, Env))
exprs [] = nonEmptyError "Denote.exprs"
exprs (e : es) = runStateT do
  v <- StateT (expr e)
  foldlM (\_ e -> StateT (expr e)) v es

module_ :: Syntax.Module -> IO Value
module_ mod =
  let funs = Map.fromList [(d.funid, funDecl funs d) | d <- mod.decls] 
   in case funs Map.!? ("main" Syntax.:/ 0) of
        Nothing -> throw NoEntryPoint
        Just main -> main []

bifs :: Set Syntax.FunId
bifs = Set.fromList
  [ "agner:print/1"
  ]

isBif :: Syntax.FunId -> Bool
isBif = (`Set.member` bifs)

bif :: Syntax.FunId -> ([Value] -> IO Value)
bif = \case
  "agner:print/1" -> \[value] -> do
    putStrLn (Value.encode value)
    pure (Value.Atom "ok")
  funid -> throw (UnknownBiF funid)

resolveFunction :: (?funs :: FunEnv) => Syntax.FunId -> ([Value] -> IO Value)
resolveFunction funid | isBif funid = bif funid
resolveFunction funid | Just f <- ?funs Map.!? funid = f
resolveFunction funid = throw (UndefinedFunction funid)