module Language.Agner.Parser (Ex(..), Parser, parse, expr, exprs, module_) where

import Data.Char qualified as Char
import Data.Void (Void)

import Control.Monad (void)
import Control.Exception (Exception, throw)

import Text.Megaparsec (Parsec, try, between, choice, runParser, eof, many, empty, (<|>))
import Text.Megaparsec.Char (char, digitChar, space1, upperChar, alphaNumChar)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error (errorBundlePretty)
import Control.Applicative.Combinators.NonEmpty (sepBy1)
import Control.Monad.Combinators.Expr (makeExprParser, Operator(..))

import Language.Agner.Syntax

type Parser = Parsec Void String

data Ex
  = ParsingException String
  deriving stock (Show)
  deriving anyclass (Exception)

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "%") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

underscore :: Parser ()
underscore = void do char '_'

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

variable :: Parser String
variable = lexeme do
  c <- upperChar
  cs <- many alphaNumChar
  pure (c:cs)

pat :: Parser Pat
pat = choice
  [ PatVar <$> variable
  , PatInteger <$> integer
  , PatWildcard <$ symbol "_"
  ]

match :: Parser (Pat, Expr)
match = do
  p <- pat
  symbol "="
  e <- expr
  pure (p, e)

term :: Parser Expr
term = choice
  [ parens expr
  , uncurry Match <$> try match
  , Integer <$> integer
  , Var <$> variable
  ]

expr :: Parser Expr
expr = makeExprParser term operatorTable
  where
    operatorTable :: [[Operator Parser Expr]]
    operatorTable =
      [ [ binary "+" (\a b -> BinOp a (:+) b) ]
      ]

    binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
    binary name f = InfixL (f <$ symbol name)

exprs :: Parser Exprs
exprs = expr `sepBy1` symbol ","

module_ :: Parser Module
module_ = exprs

parse :: Parser a -> String -> a
parse p s =
  case runParser (p <* eof) "" s of
    Right x -> x
    Left err -> throw (ParsingException (errorBundlePretty err))