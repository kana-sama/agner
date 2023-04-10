module Language.Agner.Parser (Ex(..), Parser, parse, expr, exprs, module_) where

import Language.Agner.Prelude hiding (try)

import Data.Char qualified as Char

import Text.Megaparsec (Parsec, label, satisfy, between, choice, runParser, eof, oneOf, some, notFollowedBy, many, empty, (<|>), try, sepBy, sepBy1, optional, anySingle, manyTill)
import Text.Megaparsec.Char (char, string, digitChar, space1, upperChar, lowerChar, alphaNumChar)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error (errorBundlePretty)
import Control.Monad.Combinators.Expr (makeExprParser, Operator(..))

import Language.Agner.Syntax

type Parser = Parsec Void String

data Ex
  = ParsingException String
  deriving stock (Show)

instance Exception Ex where
  displayException = \case
    ParsingException msg -> msg


ord :: Integral a => Char -> a
ord = fromIntegral . Char.ord

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "%") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

underscore :: Parser Char
underscore = char '_'

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
        ds <- many (underscore *> digit <|> digit)
        pure (toNumber (d:ds))
    , char_
    ]
  where
    toNumber :: [Int] -> Integer
    toNumber = fromIntegral . foldl (\b a -> b * 10 + a) 0

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

variable :: Parser Var
variable = lexeme do
  x <- upperChar <|> underscore
  xs <- (if x == '_' then some else many) (underscore <|> alphaNumChar <|> char '@')
  pure (x:xs)

atom :: Parser Atom
atom = lexeme do
  x <- lowerChar
  xs <- many (underscore <|> alphaNumChar)
  pure (x:xs)

pat :: Parser Pat
pat = choice
  [ PatVar <$> try variable
  , PatWildcard <$ symbol "_"
  , PatAtom <$> atom
  , PatInteger <$> patInteger
  , PatTuple <$> tupleOf pat
  , listOrListPrefix
  ]
  where
    patInteger = lexeme do
      choice
        [ integer
        , parens patInteger
        , lexeme (char '+' <* notFollowedBy (char '+')) *> patInteger
        , lexeme (char '-' <* notFollowedBy (char '-')) *> patInteger <&> negate
        ]

    listOrListPrefix = do
      a <- listOf pat <|> ((, Nothing) <$> stringOf PatInteger)
      b <- optional do symbol "++" *> pat
      case (a, b) of
        ((prefix, Nothing), Just suffix) ->
          pure (makeList PatCons suffix a)
        (list, Nothing) ->
          pure (makeList PatCons PatNil list)
        _ -> empty

match :: Parser (Pat, Expr)
match = do
  p <- pat
  symbol "="
  e <- expr
  pure (p, e)

qualifiedName :: Parser (Maybe Atom, Atom)
qualifiedName = do
  a <- atom
  b <- optional do symbol ":" *> atom
  case (a, b) of
    (a, Nothing) -> pure (Nothing, a)
    (a, Just b) -> pure (Just a, b)

fun :: Parser FunId
fun = do
  symbol "fun"
  (ns, f) <- qualifiedName
  symbol "/"
  arity <- integer
  pure (MkFunId ns f (fromInteger arity))

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

tupleOf :: Parser a -> Parser [a]
tupleOf elem = braces (elem `sepBy` symbol ",")

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

listOf :: Parser a -> Parser ([a], Maybe a)
listOf elem = brackets do
  elem `sepBy` symbol "," >>= \case
    [] -> pure ([], Nothing)
    es -> do
      rest <- optional (symbol "|" *> elem)
      pure (es, rest)

stringOf :: (Integer -> a) -> Parser [a]
stringOf elem = lexeme do
  map (elem . ord) <$> (char '"' *> manyTill L.charLiteral (char '"'))

makeList :: (a -> a -> a) -> a -> ([a], Maybe a) -> a
makeList cons nil ([], Just rest) = rest
makeList cons nil ([], Nothing) = nil
makeList cons nil (e:es, rest) = cons e (makeList cons nil (es, rest))

receive :: Parser [(Pat, Exprs)]
receive = do
  symbol "receive"
  cases <- case_ `sepBy1` symbol ";"
  symbol "end"
  pure cases
  where
    case_ = do
      p <- pat
      symbol "->"
      es <- exprs
      pure (p, es)

term :: Parser Expr
term = choice
  [ Fun <$> fun
  , Receive <$> receive
  , uncurry Match <$> try match
  , uncurry (Apply SimpleCall) <$> try apply
  , uncurry DynApply <$> try dynApply
  , parens expr
  , Var <$> variable
  , Atom <$> atom
  , Integer <$> integer
  , Tuple <$> tupleOf expr
  , makeList Cons Nil <$> listOf expr
  , makeList Cons Nil . (, Nothing) <$> stringOf Integer
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
      , 01 * [ binary "++"      []         do BinOp PlusPlus ]
      , 01 * [ binary "=<"      []         do BinOp LTE
             , binary ">="      []         do BinOp GTE ]
      , 01 * [ binary "andalso" []         do AndAlso ]
      , 01 * [ binary "orelse"  []         do OrElse  ]
      , 01 * [ binary "!"       []         do Send    ]
      ]

    (*) = replicate

    unary :: String -> [String] -> (Expr -> Expr) -> Operator Parser Expr
    unary name notNext f = Prefix (f <$ op name notNext)

    binary :: String -> [String] -> (Expr -> Expr -> Expr) -> Operator Parser Expr
    binary name notNext f = InfixL (f <$ op name notNext)

    op n notNext = (lexeme . try) (string n <* notFollowedBy (choice (map string notNext)))

exprs :: Parser Exprs
exprs = expr `sepBy1` symbol ","

guardSeq :: Parser [Expr]
guardSeq = expr `sepBy1` symbol ","

funClause :: Parser FunClause
funClause = do
  name <- atom
  pats <- parens (pat `sepBy` symbol ",")
  guards <- fromMaybe [] <$> optional (symbol "when" *> guardSeq)
  symbol "->"
  body <- exprs
  let funid = MkFunId Nothing name (length pats)
  pure MkFunClause{funid, pats, guards, body}

funDecl :: Parser FunDecl
funDecl = do
  c : cs <- funClause `sepBy1` symbol ";"
  symbol "."
  let clauses = c : cs
  let funid = c.funid
  pure MkFunDecl {funid, clauses}

module_ :: Parser Module
module_ = do
  decls <- many funDecl
  pure MkModule{decls}

parse :: Parser a -> String -> a
parse p s =
  case runParser (sc *> p <* eof) "" s of
    Right x -> x
    Left err -> throw (ParsingException (errorBundlePretty err))