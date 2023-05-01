module Language.Agner.Desugarer (desugar) where

import Language.Agner.Prelude
import Language.Agner.Syntax

import Data.Set qualified as Set
import Data.Generics.Uniplate.Data (rewriteBi, rewriteBiM, transformBi)


desugar :: Module -> Module
desugar = andAlso . orElse . comps . send . maps . operators . maybe_ . resolve


andAlso :: Module -> Module
andAlso = rewriteBi \case
  AndAlso a b -> Just do
    Case a
      [ CaseBranch (PatAtom "true")  [] [b]
      , CaseBranch (PatAtom "false") [] [Atom "false"] ]
  _ -> Nothing


orElse :: Module -> Module
orElse = rewriteBi \case
  OrElse a b -> Just do
    Case a
      [ CaseBranch (PatAtom "true")  [] [Atom "true"]
      , CaseBranch (PatAtom "false") [] [b] ]
  _ -> Nothing


applyQualifiers :: Expr -> [CompQualifier] -> Expr
applyQualifiers result = \case
  [] ->
    Cons result Nil

  CompListGenerator p e : qualifiers ->
    Apply "lists:flatmap/2"
      [ FunL [ MkClause [p] [] [applyQualifiers result qualifiers]
             , MkClause [PatWildcard] [] [Nil] ]
      , e ]

  CompMapGenerator k v e : qualifiers ->
    Apply "lists:flatmap/2"
      [ FunL [ MkClause [PatTuple [k, v]] []
                [applyQualifiers result qualifiers]
             , MkClause [PatWildcard] [] [Nil] ]
      , Apply "maps:to_list/1" [e] ]

  CompFilter p : qualifiers ->
    Case p
      [ CaseBranch (PatAtom "true") []
          [applyQualifiers result qualifiers]
      , CaseBranch PatWildcard [] [Nil] ]


comps :: Module -> Module
comps = rewriteBi \case
  ListComp result qualifiers -> Just do
    applyQualifiers result qualifiers
  MapComp k v qualifiers -> Just do
    Apply "maps:from_list/1" [applyQualifiers (Tuple [k, v]) qualifiers]
  _ -> Nothing


send :: Module -> Module
send = rewriteBi \case
  Send a b -> Just do
    Apply "erlang:send/2" [a, b]
  _ -> Nothing


maps :: Module -> Module
maps = rewriteBi \case
  Map [] -> Just do
    Apply "maps:new/0" []

  Map elems -> Just do
    let tuples = elems >>= \case
          (:=>) k v -> pure (Tuple [k, v])
          (::=) k v -> error "unexpected := in map literal"
     in Apply "maps:from_list/1" [foldr Cons Nil tuples]

  MapUpdate m [] -> Just do
    Apply "maps:assert/1" [m]

  MapUpdate m (k :=> v : upds) -> Just do
    MapUpdate (Apply "maps:put/3" [k, v, m]) upds

  MapUpdate m (k ::= v : upds) -> Just do
    MapUpdate (Apply "maps:update/3" [k, v, m]) upds

  _ -> Nothing


operators :: Module -> Module
operators = rewriteBi \case
  UnOp op a -> Just do
    Apply (fromString ("agner:" ++ unOpName op ++ "/1")) [a]

  BinOp op a b -> Just do
    Apply (fromString ("agner:" ++ binOpName op ++ "/2")) [a, b]

  _ -> Nothing


resolve :: Module -> Module
resolve module_ = module_ & transformBi \case
  funid@MkUnresolvedFunId{} ->
    if funid `Set.member` localNames
      then funid{ns = module_.name}
      else funid{ns = "erlang"}
  funid -> funid
  where
    localNames = foldMap getDeclName module_.decls
    getDeclName = \case
      Primitive{funid} -> Set.singleton MkUnresolvedFunId{name=funid.name, arity=funid.arity}
      FunDecl{funid} -> Set.singleton MkUnresolvedFunId{name=funid.name, arity=funid.arity}
      RecordDecl{} -> Set.empty


maybe_ :: Module -> Module
maybe_ = flip evalState 0 . rewriteBiM \case
  Maybe es [] -> Just <$> do
    body <- go es
    var <- fresh
    pure do
      Case (DynApply (FunL [MkClause [] [] [body]]) [])
        [ CaseBranch (PatTuple [PatAtom  "ok", PatVar var]) [] [Var var]
        , CaseBranch (PatTuple [PatAtom "err", PatVar var]) [] [Var var] ]
  
  Maybe es else_branches -> Just <$> do
    body <- go es
    var <- fresh
    else_branches <- for else_branches \CaseBranch{pat, guards, body} ->
      pure CaseBranch{pat = PatTuple [PatAtom "err", pat], guards, body}
    pure do
      Case (DynApply (FunL [MkClause [] [] [body]]) []) do
        mconcat
          [ [ CaseBranch (PatTuple [PatAtom  "ok", PatVar var]) [] [Var var] ]
          , else_branches
          , [ CaseBranch (PatTuple [PatAtom "err", PatVar var]) []
                [Apply "erlang:error/1" [Tuple [Atom "else_clause", Var var]]] ]
          ]
  _ -> pure Nothing
  where
    ok_ a = Tuple [Atom "ok", a]
    err_ a = Tuple [Atom "err", a]

    go [] = error "Desugarer.maybe: maybe block should not be empty"
    go [MaybeExpr e] = pure (ok_ e)
    go [MaybeBind p e] = do
      wc <- fresh
      pure do
        Case e
          [ CaseBranch p [] [ok_ e]
          , CaseBranch (PatVar wc) [] [err_ (Var wc)] ]
    go (MaybeExpr e : es) = do
      es <- go es
      pure (Begin [e, es])
    go (MaybeBind p e : es) = do
      wc <- fresh
      es <- go es
      pure do
        Case e
          [ CaseBranch p [] [es]
          , CaseBranch (PatVar wc) [] [err_ (Var wc)] ]

    fresh = do
      uuid <- id <<+= 1
      pure (MkVar ("_MaybeVar" ++ show uuid))
