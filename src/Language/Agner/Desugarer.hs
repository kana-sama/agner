module Language.Agner.Desugarer (desugar) where

import Language.Agner.Prelude
import Language.Agner.Syntax

import Data.Generics.Uniplate.Data (rewriteBi)

desugar :: Module -> Module
desugar = andAlso . orElse . listComp . send . maps . operators

andAlso :: Module -> Module
andAlso = rewriteBi \case
  AndAlso a b -> Just do
    Case a
      [ CaseBranch (PatAtom "true")  [] b
      , CaseBranch (PatAtom "false") [] (Atom "false") ]
  _ -> Nothing

orElse :: Module -> Module
orElse = rewriteBi \case
  OrElse a b -> Just do
    Case a
      [ CaseBranch (PatAtom "true")  [] (Atom "true")
      , CaseBranch (PatAtom "false") [] b ]
  _ -> Nothing

listComp :: Module -> Module
listComp = rewriteBi \case
  ListComp result [] -> Just do
    Cons result Nil
  ListComp result (ListCompGenerator p e : qualifiers) -> Just do
    Apply "lists:flatmap/2"
      [ FunL [ MkClause [p] [] (ListComp result qualifiers)
             , MkClause [PatWildcard] [] Nil ]
      , e ]
  ListComp result (ListCompFilter p : qualifiers) -> Just do
    Case p
      [ CaseBranch (PatAtom "true") [] (ListComp result qualifiers)
      , CaseBranch PatWildcard [] Nil ]
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
