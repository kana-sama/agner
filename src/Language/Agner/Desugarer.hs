module Language.Agner.Desugarer (desugar) where

import Language.Agner.Prelude
import Language.Agner.Syntax

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Generics.Uniplate.Data (rewriteBi, rewriteBiM, transformBi)


desugar :: Module -> Module
desugar module_ = module_
  & resolve
  & unrecord
  & validateGuards
  & (andAlso . orElse)
  & (operators . send)
  & comps
  & maps
  & maybe_


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
resolve module_ = module_
  & #decls %~ filter (isn't #_ImportDecl)
  & transformBi \case
      funid@MkUnresolvedFunId{} ->
        case localNames Map.!? funid of
          Just funid -> funid
          Nothing -> funid{ns = "erlang"}
      funid -> funid
  where
    u MkFunId{name, arity} = MkUnresolvedFunId{name, arity} -- unresolved
    ns %: MkFunId{name, arity} = MkFunId{ns, name, arity}   -- resolved

    localNames = foldMap getDeclName module_.decls
    getDeclName = \case
      Primitive{funid} -> Map.singleton (u funid) (module_.name %: funid)
      FunDecl  {funid} -> Map.singleton (u funid) (module_.name %: funid)
      RecordDecl{} -> Map.empty
      ExportDecl{} -> Map.empty
      ImportDecl{moduleName, names} -> Map.fromList [ (u funid, moduleName %: funid) | funid <- names ]


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

-- https://www.erlang.org/doc/reference_manual/expressions.html#guard-expressions
-- TODO: Expressions that construct atoms, integer, floats, lists, tuples, records, binaries, and maps
-- TODO: #Name.Field
isGuardExpr :: Expr -> Bool
isGuardExpr = \case
  Var _ -> True
  Atom _ -> True
  Integer _ -> True
  Nil -> True
  Cons a b -> isGuardExpr a && isGuardExpr b
  Tuple es -> all isGuardExpr es
  Record _ fields -> all isGuardExpr [e | (_, e) <- fields]
  Map kvs -> all (\case (:=>) k v -> isGuardExpr k && isGuardExpr v; (::=) _ _ -> False) kvs
  UnOp _ a -> isGuardExpr a
  BinOp _ a b -> isGuardExpr a && isGuardExpr b
  AndAlso a b -> isGuardExpr a && isGuardExpr b
  OrElse a b -> isGuardExpr a && isGuardExpr b
  Apply f es -> all isGuardExpr es && f `elem` funs
  RecordGet e _ _ -> isGuardExpr e
  MapUpdate e upds -> isGuardExpr e && all (\case (:=>) k v -> isGuardExpr k && isGuardExpr v; (::=) k v -> isGuardExpr k && isGuardExpr v) upds
  _ -> False
  where
    funs :: [FunId]
    funs = mconcat
      [ -- type tests
        [ "erlang:is_atom/1", "erlang:is_binary/1", "erlang:is_bitstring/1"
        , "erlang:is_boolean/1", "erlang:is_float/1", "erlang:is_function/1"
        , "erlang:is_function/2", "erlang:is_integer/1", "erlang:is_list/1"
        , "erlang:is_map/1", "erlang:is_number/1", "erlang:is_pid/1"
        , "erlang:is_port/1", "erlang:is_record/2", "erlang:is_record/3"
        , "erlang:is_reference/1", "erlang:is_tuple/1"
        ],

        -- other
        [ "erlang:abs/1", "erlang:bit_size/1", "erlang:byte_size/1"
        , "erlang:element/2", "erlang:float/1", "erlang:hd/1", "erlang:is_map_key/2"
        , "erlang:length/1", "erlang:map_get/2", "erlang:map_size/1", "erlang:node/0"
        , "erlang:node/1", "erlang:round/1", "erlang:self/0", "erlang:size/1"
        , "erlang:tl/1", "erlang:trunc/1", "erlang:tuple_size/1"
        ]
      ]

validateGuards :: Module -> Module
validateGuards = transformBi \(MkGuardExpr e) ->
  if isGuardExpr e
    then MkGuardExpr e
    else error ("invalid guard expression: " ++ show e)

unrecord :: Module -> Module
unrecord module_ = module_
  & #decls %~ filter (isn't #_RecordDecl)

  & flip evalState 0 . rewriteBiM \case
      Record record_name kvs -> Just <$> do
        let fields = recordFields record_name
        let !()    = checkFields record_name [k | (k, v) <- kvs]
        let values = [case List.lookup f kvs of Nothing -> Atom "undefined"; Just e -> e | f <- fields]
        pure (Tuple (Atom (coerce record_name) : values))

      RecordGet expr record_name record_field -> Just <$> do
        exprVar <- fresh
        let fields = length (recordFields record_name)
        let ix = recordField record_name record_field
        pure do Begin
                  [ Match (PatVar exprVar) expr
                  , Apply "agner:assert_record/3" [Var exprVar, Atom (coerce record_name), Integer (fromIntegral fields)]
                  , Apply "erlang:element/2" [Integer ix, Var exprVar]
                  ]

      RecordUpdate expr record_name kvs -> Just <$> do
        exprVar <- fresh
        let fields = length (recordFields record_name)
        pure do Begin
                  [ Match (PatVar exprVar) expr
                  , Apply "agner:assert_record/3" [Var exprVar, Atom (coerce record_name), Integer (fromIntegral fields)]
                  , foldr
                      (\(field, val) r -> Apply "agner:update_element/3" [Integer (recordField record_name field), r, val])
                      (Var exprVar) kvs
                  ]

      RecordSelector record_name record_field -> Just <$> do
        pure (Integer (recordField record_name record_field))

      _ -> pure Nothing

  & rewriteBi \case
      PatRecord record_name kvs -> Just do
        let fields = recordFields record_name
        let !()    = checkFields record_name [k | (k, v) <- kvs]
        let values = [case List.lookup f kvs of Nothing -> PatWildcard; Just p -> p | f <- fields]
        PatTuple (PatAtom (coerce record_name) : values)

      _ -> Nothing
  where
    fresh :: State Int Var
    fresh = do
      uuid <- id <<+= 1
      pure (MkVar ("_RecordVar" ++ show uuid))

    records = Map.fromList [(name, fields) | RecordDecl name fields <- module_.decls]

    checkFields record_name fields =
      let valid_fields = recordFields record_name
       in case List.find (`notElem` valid_fields) fields of
            Nothing -> ()
            Just f -> error ("field " ++ f.getString ++ " undefined in record " ++ record_name.getString)

    recordFields record_name =
      case records Map.!? record_name of
        Just fields -> fields
        Nothing -> error ("record " ++ record_name.getString ++ " undefined")

    recordField record_name record_field =
      case List.findIndex (== record_field) (recordFields record_name) of
        Nothing -> error ("field " ++ record_field.getString ++ " undefined in record " ++ record_name.getString)
        Just i -> fromIntegral (i + 2)
