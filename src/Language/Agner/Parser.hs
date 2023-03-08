module Language.Agner.Parser (Ex(..), Parser, parse, expr, exprs, module_) where

import Language.Agner.Prelude hiding (try)

import Data.Char qualified as Char

import Text.Megaparsec (Parsec, between, choice, runParser, eof, some, many, empty, (<|>), try, sepBy, sepBy1, optional)
import Text.Megaparsec.Char (char, digitChar, space1, upperChar, lowerChar, alphaNumChar)
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

integer :: Parser Integer
integer = lexeme do
  d <- digit
  ds <- many (underscore *> digit <|> digit)
  pure (toNumber (d:ds))
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
  , PatInteger <$> integer
  , PatTuple <$> tupleOf pat
  , makeList PatCons PatNil <$> listOf pat
  ]

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

makeList :: (a -> a -> a) -> a -> ([a], Maybe a) -> a
makeList cons nil ([], Just rest) = rest
makeList cons nil ([], Nothing) = nil
makeList cons nil (e:es, rest) = cons e (makeList cons nil (es, rest))

term :: Parser Expr
term = choice
  [ Fun <$> fun
  , uncurry Match <$> try match
  , uncurry (Apply SimpleCall) <$> try apply
  , uncurry DynApply <$> try dynApply
  , parens expr
  , Var <$> variable
  , Atom <$> atom
  , Integer <$> integer
  , Tuple <$> tupleOf expr
  , makeList Cons Nil <$> listOf expr
  ]

expr :: Parser Expr
expr = makeExprParser term operatorTable
  where
    operatorTable :: [[Operator Parser Expr]]
    operatorTable =
      [ [ binary "+" (\a b -> BinOp a (:+) b)
        , binary "-" (\a b -> BinOp a (:-) b) ] ]

    binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
    binary name f = InfixL (f <$ symbol name)

exprs :: Parser Exprs
exprs = expr `sepBy1` symbol ","

funClause :: Parser FunClause
funClause = do
  name <- atom
  pats <- parens (pat `sepBy` symbol ",")
  symbol "->"
  body <- exprs
  let funid = MkFunId Nothing name (length pats)
  pure MkFunClause{funid, pats, body}

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