module Language.Agner.Parser (Ex(..), Parser, parse, expr, exprs, module_) where

import Data.Char qualified as Char
import Data.Void (Void)
import Data.List.NonEmpty (NonEmpty ((:|)))

import Control.Monad (void)
import Control.Exception (Exception (..), throw)

import Text.Megaparsec (Parsec, between, choice, runParser, eof, many, empty, (<|>), try, sepBy)
import Text.Megaparsec.Char (char, digitChar, space1, upperChar, lowerChar, alphaNumChar)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error (errorBundlePretty)
import Control.Applicative.Combinators.NonEmpty (sepBy1)
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
  x <- upperChar
  xs <- many (underscore <|> alphaNumChar)
  pure (x:xs)

atom :: Parser Atom
atom = lexeme do
  x <- lowerChar
  xs <- many (underscore <|> alphaNumChar)
  pure (x:xs)

pat :: Parser Pat
pat = choice
  [ PatVar <$> variable
  , PatAtom <$> atom
  , PatInteger <$> integer
  , PatWildcard <$ symbol "_"
  ]

match :: Parser (Pat, Expr)
match = do
  p <- pat
  symbol "="
  e <- expr
  pure (p, e)

apply :: Parser (Atom, [Expr])
apply = do
  f <- atom
  args <- parens (expr `sepBy` symbol ",")
  pure (f, args)

term :: Parser Expr
term = choice
  [ parens expr
  , uncurry Match <$> try match
  , uncurry Apply <$> try apply
  , Var <$> variable
  , Atom <$> atom
  , Integer <$> integer
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
  let funid = name :/ length pats
  pure MkFunClause{funid, pats, body}

funDecl :: Parser FunDecl
funDecl = do
  c :| cs <- funClause `sepBy1` symbol ";"
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