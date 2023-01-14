module Language.Agner.Parser where

import Control.Applicative ((<|>), empty, many)
import Text.Megaparsec (Parsec, between, choice, runParser, eof)
import Text.Megaparsec.Char (char, digitChar, space1)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error (errorBundlePretty)

import Language.Agner.Syntax
import Data.Void (Void)
import Control.Monad.Combinators.Expr (makeExprParser, Operator(..))
import Control.Monad (void)
import Data.Char qualified as Char

type Parser = Parsec Void String

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

term :: Parser Expr
term = choice
  [ parens expr
  , Integer <$> integer
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

parse :: String -> Expr
parse s =
  case runParser (expr <* eof) "" s of
    Right x -> x
    Left err -> error (errorBundlePretty err)