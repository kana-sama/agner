module Language.Agner.Prettier where

import Language.Agner.Prelude
import Language.Agner.Parser qualified as Parser
import Language.Agner.Syntax

import Prettyprinter
import Prettyprinter.Render.String
import Prettyprinter.Render.Terminal

import System.IO (stdout)
import Data.Map.Strict qualified as Map

type D = Doc AnsiStyle

colors :: [Color]
colors = [Magenta, Green, Blue, Yellow, Red, Cyan]

type WithVarColors = (?varColor :: Var -> Color)

withVarColors :: [Var] -> (WithVarColors => a) -> a
withVarColors vars k = do
  let map = Map.fromList (zip vars (cycle colors))
  let ?varColor = \var -> map Map.! var
  k

binop :: BinOp -> D
binop = \case
  (:+) -> "+"
  (:-) -> "-"

funId :: FunId -> D
funId funid =
  let ns = pretty case funid.ns of Just ns -> ns ++ ":"; Nothing -> mempty
   in ns <> pretty funid.name

var :: WithVarColors => Var -> D
var v = annotate (color (?varColor v)) (pretty v)

expr :: WithVarColors => Expr -> D
expr = \case
  Integer i -> pretty i
  Atom a -> pretty a
  Fun f -> "fun" <+> funId f <> "/" <> pretty f.arity
  BinOp l op r -> expr l <+> binop op <+> expr r
  Var v -> var v
  Match p e -> pat p <+> "=" <+> expr e
  Apply _ f es -> funId f <> parens (listOf expr es)
  DynApply (Var v) es -> var v <> parens (listOf expr es)
  DynApply e es -> parens (expr e) <> parens (listOf expr es)

pat :: WithVarColors => Pat -> D
pat = \case
  PatVar v -> var v
  PatWildcard -> "_"
  PatInteger i -> pretty i
  PatAtom a -> pretty a

listOf :: (a -> D) -> [a] -> D
listOf p xs = hsep (punctuate comma (p <$> xs))

linesOf :: (a -> D) -> [a] -> D
linesOf p xs = vsep (punctuate comma (p <$> xs))

clause :: FunClause -> D
clause c = do
  withVarColors (toList (funClauseVars c)) do
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