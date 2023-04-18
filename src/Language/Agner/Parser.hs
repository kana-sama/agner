module Language.Agner.Parser
  -- (Ex(..), Parser, parse, expr, exprs, module_)
  where

import Language.Agner.Prelude hiding (try)

import Data.Char qualified as Char

import Text.Megaparsec (Parsec, label, satisfy, between, choice, runParser, eof, oneOf, some, notFollowedBy, many, empty, (<|>), try, sepBy, sepBy1, optional, anySingle, manyTill)
import Text.Megaparsec.Char (char, string, digitChar, space1, upperChar, lowerChar, alphaNumChar)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error (errorBundlePretty)
import Control.Monad.Combinators.Expr (makeExprParser, Operator(..))

import Language.Agner.Syntax

type Parser = Parsec Void String

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
  Arg _ -> error "invalid pattern: arg"
  Var "_" -> PatWildcard
  Var v -> PatVar v
  Fun _ -> error "invalid pattern: function"
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

fullyQualifiedName :: Parser FunId
fullyQualifiedName = do
  ns <- atom
  symbol ":"
  name <- atom
  symbol "/"
  arity <- integer
  pure (MkFunId (coerce ns) (coerce name) (fromInteger arity))

qualifiedName :: Parser (ModuleName, FunName)
qualifiedName = do
  a <- atom
  b <- optional do symbol ":" *> atom
  case (a, b) of
    (a, Nothing) -> pure ("root", coerce a)
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

makeList :: ([Expr], Maybe Expr) -> Expr
makeList ([], Just rest) = rest
makeList ([], Nothing) = Nil
makeList (e:es, rest) = Cons e (makeList (es, rest))

caseBranch :: Parser CaseBranch
caseBranch = do
  p <- pat
  symbol "->"
  es <- exprs
  pure (CaseBranch p es)

case_ :: Parser (Expr, [CaseBranch])
case_ = do
  symbol "case"
  e <- expr
  symbol "of"
  bs <- caseBranch `sepBy1` symbol ";"
  symbol "end"
  pure (e, bs)

receive :: Parser [CaseBranch]
receive = do
  symbol "receive"
  bs <- caseBranch `sepBy1` symbol ";"
  symbol "end"
  pure bs

begin :: Parser Expr
begin = between (symbol "begin") (symbol "end") exprs

term :: Parser Expr
term = choice
  [ Fun <$> fun
  , Receive <$> receive
  , uncurry Case <$> case_
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
  ]

expr :: Parser Expr
expr = makeExprParser term operatorTable
  where
    operatorTable :: [[Operator Parser Expr]]
    operatorTable = concat
      [ 64 * [ unary  "+"       ["+"]      do UnOp  Plus'
             , unary  "-"       ["-"]      do UnOp  Minus'
             , unary  "bnot"    []         do UnOp  BNot
             , unary  "not"     []         do UnOp  Not ]
      , 01 * [ binary "*"       []         do BinOp Times
             , binary "div"     []         do BinOp Div
             , binary "rem"     []         do BinOp Rem
             , binary "band"    []         do BinOp BAnd
             , binary "and"     ["also"]   do BinOp And ]
      , 01 * [ binary "+"       ["+"]      do BinOp Plus
             , binary "-"       ["-", ">"] do BinOp Minus
             , binary "bor"     []         do BinOp BOr
             , binary "bxor"    []         do BinOp BXor
             , binary "bsl"     []         do BinOp BSL
             , binary "bsr"     []         do BinOp BSR
             , binary "or"      ["else"]   do BinOp Or
             , binary "xor"     []         do BinOp Xor ]
      , 01 * [ binary "++"      []         do BinOp Plus_Plus
             , binary "--"      []         do BinOp Minus_Minus ]
      , 01 * [ binary "=="      []         do BinOp Eq_Eq
             , binary "/="      []         do BinOp Slash_Eq
             , binary "=<"      []         do BinOp Eq_Less
             , binary "<"       []         do BinOp Less
             , binary ">="      []         do BinOp Greater_Eq
             , binary ">"       []         do BinOp Greater
             , binary "=:="     []         do BinOp Eq_Colon_Eq
             , binary "=/="     []         do BinOp Eq_Slash_Eq ]
      , 01 * [ binary "andalso" []         do andAlso ]
      , 01 * [ binary "orelse"  []         do orElse  ]
      , 01 * [ binary "!"       []         do send
             , binary "="       []         do match ]
      ]

    andAlso a b = Case a
      [ CaseBranch (PatAtom "true") b
      , CaseBranch (PatAtom "false") (Atom "false") ]
    orElse a b = Case a
      [ CaseBranch (PatAtom "true") (Atom "true")
      , CaseBranch (PatAtom "false") b ]
    match a b = Match (exprToPat a) b
    send  a b = Apply "erlang:send/2" [a, b]

    (*) = replicate

    unary :: String -> [String] -> (Expr -> Expr) -> Operator Parser Expr
    unary name notNext f = Prefix (f <$ op name notNext)

    binary :: String -> [String] -> (Expr -> Expr -> Expr) -> Operator Parser Expr
    binary name notNext f = InfixL (f <$ op name notNext)

    op n notNext = (lexeme . try) (string n <* notFollowedBy (choice (map string notNext)))

exprs :: Parser Expr
exprs = foldr1 Seq <$> expr `sepBy1` symbol ","

funClause :: Parser ((FunName, Int), ([Pat], Expr))
funClause = do
  name <- coerce <$> atom
  pats <- parens (pat `sepBy` symbol ",")
  symbol "->"
  body <- exprs
  pure ((name, length pats), (pats, body))

funDecl :: Parser Decl
funDecl = do
  (sigs@((name, arity):_), clauses) <- unzip <$> funClause `sepBy1` symbol ";"
  symbol "."
  guard (all (== (name, arity)) sigs)
  let funid = MkFunId "root" name arity
  let body = case clauses of
        -- one nullary clause
        [([], expr)] -> expr
        -- some unary clauses
        ([_], _):_ ->
          Case (Arg 0)
            [CaseBranch p e | ([p], e) <- clauses]
        -- some n-ary clauses
        clauses ->
          Case (Tuple [Arg i | i <- [0..arity - 1]])
            [CaseBranch (PatTuple ps) e | (ps, e) <- clauses]
  pure FunDecl {funid, body}

decl :: Parser Decl
decl = choice
  [ funDecl
  , pragma "native" do
      agner <- fullyQualifiedName
      symbol ","
      c <- stringLit
      pure Native{agner, c}
  ]
  where
    pragma name body = do
      symbol "-"
      symbol name
      value <- parens body
      symbol "."
      pure value

module_ :: Parser Module
module_ = do
  decls <- many decl
  pure MkModule{decls}

parse :: Parser a -> String -> a
parse p s =
  case runParser (sc *> p <* eof) "" s of
    Right x -> x
    Left err -> error (errorBundlePretty err)
