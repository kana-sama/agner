module Language.Agner.Prettier where

import Language.Agner.Prelude
import Language.Agner.Parser qualified as Parser
import Language.Agner.Syntax

import Prettyprinter
import Prettyprinter.Render.String
import Prettyprinter.Render.Terminal

import System.IO (stdout)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

type D = Doc AnsiStyle

type WithVarStyles = (?varStyle :: Var -> AnsiStyle)

withVarStyles :: [Var] -> (WithVarStyles => a) -> a
withVarStyles vars k = do
  let map = Map.fromList (zip vars (cycle styles))
  let ?varStyle = \var -> map Map.! var
  k
  where
    colors = [Blue, Green, Magenta, Yellow, Red, Cyan, Black, White]
    styles = concat
      [ [color c | c <- colors, c /= White]
      , [bgColor c | c <- colors]
      , [color c <> italicized <> underlined | c <- colors, c /= White]
      , [bgColor c <> italicized <> underlined | c <- colors]
      ]

binop :: BinOp -> D
binop = \case
  (:+) -> "+"
  (:-) -> "-"

funId :: FunId -> D
funId funid =
  let ns = pretty case funid.ns of Just ns -> ns ++ ":"; Nothing -> mempty
   in ns <> pretty funid.name

var :: WithVarStyles => Var -> D
var v = annotate (?varStyle v) (pretty v)

expr :: WithVarStyles => Expr -> D
expr = \case
  Integer i -> pretty i
  Atom a -> pretty a
  Fun f -> "fun" <+> funId f <> "/" <> pretty f.arity
  Tuple es -> braces (hsep (punctuate comma (expr <$> es)))
  BinOp l op r -> expr l <+> binop op <+> expr r
  Var v -> var v
  Match p e -> pat p <+> "=" <+> expr e
  Apply _ f es -> funId f <> parens (listOf expr es)
  DynApply (Var v) es -> var v <> parens (listOf expr es)
  DynApply e es -> parens (expr e) <> parens (listOf expr es)

pat :: WithVarStyles => Pat -> D
pat = \case
  PatVar v -> var v
  PatWildcard -> "_"
  PatInteger i -> pretty i
  PatAtom a -> pretty a
  PatTuple ps -> braces (hsep (punctuate comma (pat <$> ps)))

listOf :: (a -> D) -> [a] -> D
listOf p xs = hsep (punctuate comma (p <$> xs))

linesOf :: (a -> D) -> [a] -> D
linesOf p xs = vsep (punctuate comma (p <$> xs))

clause :: FunClause -> D
clause c = do
  withVarStyles (Set.toList (funClauseVars c)) do
    funId c.funid <> parens (listOf pat c.pats) <+> "->" <+>
      nest 2 (group (line' <> vcat (punctuate ", " (expr <$> c.body))))

decl :: FunDecl -> D
decl d = vsep (punctuate ";" (clause <$> d.clauses)) <> "."

module_ :: Module -> D
module_ m = vsep (punctuate line (decl <$> m.decls)) <> hardline

prettyOpts :: LayoutOptions
prettyOpts = LayoutOptions (AvailablePerLine 40 1)

io :: (a -> D) -> a -> IO ()
io p x = renderIO stdout (layoutPretty prettyOpts (p x))

string :: (a -> D) -> a -> String
string p x = renderString (layoutPretty prettyOpts (p x))