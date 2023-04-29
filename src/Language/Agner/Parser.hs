{-# LANGUAGE DataKinds #-}

module Language.Agner.Parser where

import Language.Agner.Prelude hiding (try)

import Data.Char qualified as Char
import GHC.Records (HasField(getField))

import Text.Megaparsec (ParsecT, label, satisfy, between, choice, runParserT, eof, oneOf, some, notFollowedBy, many, empty, (<|>), try, sepBy, sepBy1, optional, anySingle, manyTill)
import Text.Megaparsec.Char (char, string, digitChar, space1, upperChar, lowerChar, alphaNumChar)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error (errorBundlePretty)
import Control.Monad.Combinators.Expr (makeExprParser, Operator(..))

import Language.Agner.Syntax

type Parser = ParsecT Void String (State ModuleName)

instance HasField "length" [a] Int where
  getField = length

ord :: Integral a => Char -> a
ord = fromIntegral . Char.ord

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "%") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

digit :: Parser Int
digit = (\c -> Char.ord c - Char.ord '0') <$> digitChar

char_ :: Parser Integer
char_ = lexeme do
  char '$'
  escaped <|> not_escaped
  where
    not_escaped = ord <$> satisfy (/= '\\')
    octal = do
      a <- octalDigit
      b <- octalDigit
      c <- octalDigit
      pure (a*8*8 + b*8 + c)
    octalDigit = (\c -> ord c - ord '0') <$> oneOf ['0'..'7']
    escaped = char '\\' *> choice
      [ octal
      , string "n"  $> ord '\n'
      , string "r"  $> ord '\r'
      , string "t"  $> ord '\t'
      , string "v"  $> ord '\v'
      , string "b"  $> ord '\b'
      , string "f"  $> ord '\f'
      , string "e"  $> 27
      , string "\\" $> ord '\\'
      ]

integer :: Parser Integer
integer = label "integer" do
  choice
    [ lexeme do
        d <- digit
        ds <- many (char '_' *> digit <|> digit)
        pure (toNumber (d:ds))
    , char_
    ]
  where
    toNumber :: [Int] -> Integer
    toNumber = fromIntegral . foldl (\b a -> b * 10 + a) 0

parens, braces, brackets :: Parser a -> Parser a
parens   = between (symbol "(") (symbol ")")
braces   = between (symbol "{") (symbol "}")
brackets = between (symbol "[") (symbol "]")

variable :: Parser Var
variable = lexeme do
  x <- char '_' <|> upperChar
  xs <- many (char '_' <|> char '@' <|> alphaNumChar)
  pure (MkVar (x:xs))

atom :: Parser Atom
atom = lexeme do
  x <- lowerChar
  xs <- many (char '_' <|> alphaNumChar)
  pure (MkAtom (x:xs))

exprToPat :: Expr -> Pat
exprToPat = \case
  Integer i -> PatInteger i
  Atom a -> PatAtom a
  Tuple es -> PatTuple [exprToPat e | e <- es]
  Nil -> PatNil
  Cons a b -> PatCons (exprToPat a) (exprToPat b)

  Map elems -> error "unimplemented: maps in patterns"
  MapUpdate _ _ -> error "illegal pattern: map update"

  Record recordName values -> PatRecord recordName [(f, exprToPat e) | (f, e) <- values]
  RecordGet _ _ _ -> error "invalid pattern: record_get"
  RecordUpdate _ _ _ -> error "invalid pattern: record_set"

  Arg _ -> error "invalid pattern: arg"
  Var "_" -> PatWildcard
  Var v -> PatVar v
  Fun _ -> error "invalid pattern: function"
  FunL _ _ -> error "invalid pattern: lambda"
  BinOp Plus_Plus a b | Just xs <- isKnownList a ->
    foldr PatCons (exprToPat b) xs
  BinOp _ _ _ -> error "invalid pattern: binop"
  UnOp Minus' a | Just i <- isKnownInteger a -> PatInteger i
  UnOp Plus' a | Just i <- isKnownInteger a -> PatInteger i
  UnOp _ _ -> error "invalid pattern: unop"
  Match a b -> PatMatch a (exprToPat b)
  Apply _ _ -> error "invalid pattern: apply"
  TailApply _ _ -> error "invalid pattern: tailapply"
  DynApply _ _ -> error "invalid pattern: dynapply"
  Case _ _ -> error "invalid pattern: case"
  Receive _ -> error "invalid pattern: receive"
  Seq _ _ -> error "invalid pattern: seq"
  where
    isKnownList = \case
      Nil -> Just []
      Cons (Integer i) b -> (PatInteger i :) <$> isKnownList b
      _ -> Nothing

    isKnownInteger = \case
      Integer i -> Just i
      UnOp Minus' a -> negate <$> isKnownInteger a
      UnOp Plus' a -> isKnownInteger a
      _ -> Nothing

pat :: Parser Pat
pat = exprToPat <$> expr

localName :: Parser FunId
localName = do
  ns <- get
  name <- coerce <$> atom
  symbol "/"
  arity <- lexeme L.decimal
  pure MkFunId{ns, name, arity}

qualifiedName :: Parser (ModuleName, FunName)
qualifiedName = do
  a <- atom
  b <- optional do symbol ":" *> atom
  case (a, b) of
    (a, Nothing) -> do
      moduleName <- get
      pure (moduleName, coerce a)
    (a, Just b) -> pure (coerce a, coerce b)

fun :: Parser FunId
fun = do
  symbol "fun"
  (ns, f) <- qualifiedName
  symbol "/"
  arity <- integer
  pure (MkFunId ns f (fromInteger arity))

tuple :: Parser [Expr]
tuple = braces (expr `sepBy` symbol ",")

apply :: Parser (FunId, [Expr])
apply = do
  (ns, f) <- qualifiedName
  args <- parens (expr `sepBy` symbol ",")
  pure (MkFunId ns f (length args), args)

dynApply :: Parser (Expr, [Expr])
dynApply = do
  f <- parens expr <|> Var <$> variable
  args <- parens (expr `sepBy` symbol ",")
  pure (f, args)

list :: Parser ([Expr], Maybe Expr)
list = brackets do
  expr `sepBy` symbol "," >>= \case
    [] -> pure ([], Nothing)
    es -> do
      rest <- optional (symbol "|" *> expr)
      pure (es, rest)

stringLit :: Parser String
stringLit = char '"' *> manyTill L.charLiteral (char '"')

string_ :: Parser [Expr]
string_ = lexeme do
  map (Integer . ord) <$> stringLit

map_ :: Parser [MapElemBind]
map_ = between (symbol "#{") (symbol "}") (elem `sepBy` symbol ",") where
  bind = choice [ (:=>) <$ symbol "=>", (::=) <$ symbol ":=" ]
  elem = do key <- expr; b <- bind; val <- expr; pure (b key val)

makeList :: ([Expr], Maybe Expr) -> Expr
makeList ([], Just rest) = rest
makeList ([], Nothing) = Nil
makeList (e:es, rest) = Cons e (makeList (es, rest))

guard_seq :: Parser GuardSeq
guard_seq = (expr `sepBy1` symbol ",") `sepBy1` symbol ";"

whenGuards :: Parser GuardSeq
whenGuards = do
  optional (symbol "when" *> guard_seq) >>= \case
    Nothing -> pure []
    Just gs -> pure gs

caseBranch :: Parser CaseBranch
caseBranch = do
  pat <- pat
  guards <- whenGuards
  symbol "->"
  body <- exprs
  pure CaseBranch{pat, guards, body}

case_ :: Parser (Expr, [CaseBranch])
case_ = do
  symbol "case"
  e <- expr
  symbol "of"
  bs <- caseBranch `sepBy1` symbol ";"
  symbol "end"
  pure (e, bs)

if_ :: Parser [(GuardSeq, Expr)]
if_ = symbol "if" *> (if_branch `sepBy1` symbol ";") <* symbol "end" where
  if_branch = do
    g <- guard_seq
    symbol "->"
    b <- exprs
    pure (g, b)

receive :: Parser [CaseBranch]
receive = do
  symbol "receive"
  bs <- caseBranch `sepBy1` symbol ";"
  symbol "end"
  pure bs

begin :: Parser Expr
begin = between (symbol "begin") (symbol "end") exprs

record_kv :: Parser (RecordField, Expr)
record_kv = do
  field <- coerce <$> atom
  symbol "="
  value <- expr
  pure (field, value) 

record_construct :: Parser Expr
record_construct = do
  symbol "#"
  recordName <- coerce <$> atom
  values <- braces (record_kv `sepBy` symbol ",")
  pure (Record recordName values)

funL :: Parser (Int, Expr)
funL = do
  symbol "fun"
  clauses <- clause `sepBy1` symbol ";"
  symbol "end"
  pure ((head clauses).pats.length, clausesToCase clauses)

term :: Parser Expr
term = choice
  [ Fun <$> try fun
  , uncurry FunL <$> funL
  , Receive <$> receive
  , uncurry Case <$> case_
  , makeIf <$> if_
  , uncurry Apply <$> try apply
  , uncurry DynApply <$> try dynApply
  , begin
  , parens expr
  , Var <$> variable
  , Atom <$> atom
  , Integer <$> integer
  , Tuple <$> tuple
  , makeList <$> list
  , makeList . (, Nothing) <$> string_

  , try record_construct

  , Map <$> map_
  ]
  where
    makeIf branches = Case Nil [CaseBranch PatWildcard gs body | (gs, body) <- branches]

operatorName :: Parser Language.Agner.Syntax.Operator
operatorName = choice (concat (operatorTable unary binary)) where
  hole = Var "_"
  unary  s _ build = eject (build hole)      <* try (lexeme (string (s ++ "/1")))
  binary s _ build = eject (build hole hole) <* try (lexeme (string (s ++ "/2")))
  eject = \case BinOp op _ _ -> pure (Binary op); UnOp op _ -> pure (Unary op); _ -> empty

operatorTable ::
  (String -> [String] -> (Expr -> Expr) -> a) ->
  (String -> [String] -> (Expr -> Expr -> Expr) -> a) ->
  [[a]]
operatorTable unary binary = concat
  [ 64 *  [ unary  "+"       ["+"]      do UnOp  Plus'
          , unary  "-"       ["-"]      do UnOp  Minus'
          , unary  "bnot"    []         do UnOp  BNot
          , unary  "not"     []         do UnOp  Not ]
  ,  1 *  [ binary "*"       []         do BinOp Times
          , binary "div"     []         do BinOp Div
          , binary "rem"     []         do BinOp Rem
          , binary "band"    []         do BinOp BAnd
          , binary "and"     ["also"]   do BinOp And ]
  ,  1 *  [ binary "+"       ["+"]      do BinOp Plus
          , binary "-"       ["-", ">"] do BinOp Minus
          , binary "bor"     []         do BinOp BOr
          , binary "bxor"    []         do BinOp BXor
          , binary "bsl"     []         do BinOp BSL
          , binary "bsr"     []         do BinOp BSR
          , binary "or"      ["else"]   do BinOp Or
          , binary "xor"     []         do BinOp Xor ]
  ,  1 *  [ binary "++"      []         do BinOp Plus_Plus
          , binary "--"      []         do BinOp Minus_Minus ]
  ,  1 *  [ binary "=="      []         do BinOp Eq_Eq
          , binary "/="      []         do BinOp Slash_Eq
          , binary "=<"      []         do BinOp Eq_Less
          , binary "<"       []         do BinOp Less
          , binary ">="      []         do BinOp Greater_Eq
          , binary ">"       []         do BinOp Greater
          , binary "=:="     []         do BinOp Eq_Colon_Eq
          , binary "=/="     []         do BinOp Eq_Slash_Eq ]
  ,  1 *  [ binary "andalso" []         do andAlso ]
  ,  1 *  [ binary "orelse"  []         do orElse  ]
  ,  1 *  [ binary "!"       []         do send
          , binary "="       [">"]      do match ]
  ]
  where
    (*) = replicate
    andAlso a b = Case a
      [ CaseBranch (PatAtom "true")  [] b
      , CaseBranch (PatAtom "false") [] (Atom "false") ]
    orElse a b = Case a
      [ CaseBranch (PatAtom "true")  [] (Atom "true")
      , CaseBranch (PatAtom "false") [] b ]
    match a b = Match (exprToPat a) b
    send  a b = Apply "erlang:send/2" [a, b]

expr :: Parser Expr
expr = makeExprParser term ([[mapUpdate, recordGet, recordUpdate]] ++ operatorTable unary binary) where
  unary  name notNext f = Prefix (f <$ op name notNext)
  binary name notNext f = InfixL (f <$ op name notNext)
  op n notNext = (lexeme . try) (string n <* notFollowedBy (choice (map string notNext)))

  mapUpdate = (Postfix . try) do
    update <- map_
    pure \e -> MapUpdate e update

  recordGet = (Postfix . try) do
    symbol "#"
    recordName <- coerce <$> atom
    symbol "."
    recordField <- coerce <$> atom
    pure \e -> RecordGet e recordName recordField

  recordUpdate = Postfix do
    symbol "#"
    recordName <- coerce <$> atom
    kvs <- braces (record_kv `sepBy` symbol ",")
    pure \e -> RecordUpdate e recordName kvs

exprs :: Parser Expr
exprs = foldr1 Seq <$> expr `sepBy1` symbol ","

data Clause = MkClause{pats :: [Pat], guards :: [[Expr]], body :: Expr}

clausesToCase :: [Clause] -> Expr
clausesToCase = \case
  [] -> error "clauses should be nonempty"
  -- one nullary clause
  [MkClause{pats = [], guards = [], body}] -> body
  -- some unary clauses
  clauses@(MkClause{pats = [_]}:_) ->
    Case (Arg 0)
      [CaseBranch p guards body | MkClause{pats = [p], guards, body} <- clauses]
  -- some n-ary clauses
  clauses@(MkClause{pats}:_) ->
    Case (Tuple [Arg i | i <- [0..pats.length - 1]])
      [CaseBranch (PatTuple c.pats) c.guards c.body | c <- clauses]

clause :: Parser Clause
clause = do
  pats <- parens (pat `sepBy` symbol ",")
  guards <- whenGuards
  symbol "->"
  body <- exprs
  pure MkClause{pats, guards, body}

funClause :: Parser (FunId, Clause)
funClause = do
  (ns, name) <- qualifiedName
  clause <- clause
  let funid = MkFunId ns name clause.pats.length
  pure (funid, clause)

funDecl :: Parser Decl
funDecl = do
  (sigs@(funid:_), clauses) <- unzip <$> funClause `sepBy1` symbol ";"
  symbol "."
  guard (all (== funid) sigs)
  let body = clausesToCase clauses
  pure FunDecl{funid, body}

pragma :: String -> Parser a -> Parser a
pragma name body = try do
  symbol "-"
  symbol name
  value <- parens body
  symbol "."
  pure value

decl :: Parser Decl
decl = funDecl <|> builtin <|> primitive <|> record where
  builtin = pragma "builtin" do
    name <- (.getString) <$> atom
    symbol ","
    funid <- localName
    pure BuiltIn{name, funid}

  primitive = pragma "primitive" do
    funid <- localName
    pure Primitive{funid}

  record = pragma "record" do
    recordName <- coerce <$> atom
    symbol ","
    recordFields <- coerce <$> braces (atom `sepBy` symbol ",")
    pure RecordDecl{recordName, recordFields}

module_ :: Parser Module
module_ = do
  name <- pragma "module" (coerce <$> atom)
  put name
  decls <- many decl
  pure MkModule{name, decls}

parse :: FilePath -> Parser a -> String -> a
parse path p s =
  case evalState (runParserT (sc *> p <* eof) path s) "main" of
    Right x -> x
    Left err -> error (errorBundlePretty err)
