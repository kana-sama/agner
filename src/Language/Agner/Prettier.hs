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

-- (:++) -> "++"
binop :: BinOp -> D
binop = \case
  Plus -> "+"
  Minus -> "-"
  Times -> "*"
  PlusPlus -> "++"
  GTE -> ">="
  LTE -> "=<"
  op -> pretty (binOpName op)

unop :: UnOp -> D
unop = \case
  Plus' -> "+"
  Minus' -> "-"
  op -> pretty (unOpName op)

keyword :: D -> D
keyword = annotate bold

funId :: FunId -> D
funId f =
  let ns = pretty case f.ns of Just ns -> ns ++ ":"; Nothing -> mempty
   in ns <> pretty f.name

funIdA :: FunId -> D
funIdA f = funId f <> "/" <> pretty f.arity

var :: WithVarStyles => Var -> D
var v = annotate (?varStyle v) (pretty v)

expr :: WithVarStyles => Expr -> D
expr = \case
  Integer i -> pretty i
  Atom a -> pretty a
  Fun f -> keyword "fun" <+> funIdA f
  List es -> brackets (hsep (punctuate comma (expr <$> es)))
  Nil -> "[]"
  Cons a b -> brackets (expr a <+> "|" <+> expr b)
  Tuple es -> braces (hsep (punctuate comma (expr <$> es)))
  UnOp op l -> unop op <> parens (expr l)
  BinOp op l r -> parens (expr l <+> binop op <+> expr r)
  AndAlso l r -> parens (expr l <+> "andalso" <+> expr r)
  OrElse  l r -> parens (expr l <+> "orelse"  <+> expr r)
  Var v -> var v
  Match p e -> pat p <+> "=" <+> expr e
  Apply _ f es -> funId f <> parens (listOf expr es)
  DynApply (Var v) es -> var v <> parens (listOf expr es)
  DynApply e es -> parens (expr e) <> parens (listOf expr es)
  Send l r -> parens (expr l <+> "!" <+> expr r)
  Receive cases ->
    let case_ (p, es) = pat p <+> "->" <+> nest 2 (line' <> exprs es)
        cases' = vcat (punctuate "; " (case_ <$> cases))
     in "receive" <+> vsep [nest 2 (line' <> cases'), "end"]
  Begin es ->
    "begin" <+> group (nest 2 (line' <> exprs es) <+> line' <> "end")

exprs :: WithVarStyles => Exprs -> D
exprs es = vcat (punctuate ", " (expr <$> es))

pat :: WithVarStyles => Pat -> D
pat = \case
  PatVar v -> var v
  PatWildcard -> "_"
  PatInteger i -> pretty i
  PatAtom a -> pretty a
  PatTuple ps -> braces (hsep (punctuate comma (pat <$> ps)))
  PatList ps -> brackets (hsep (punctuate comma (pat <$> ps)))
  PatNil -> "[]"
  PatCons a b -> brackets (pat a <+> "|" <+> pat b)

listOf :: (a -> D) -> [a] -> D
listOf p xs = hsep (punctuate comma (p <$> xs))

linesOf :: (a -> D) -> [a] -> D
linesOf p xs = vsep (punctuate comma (p <$> xs))

guardSeq :: WithVarStyles => [Expr] -> D
guardSeq = listOf expr

clause :: FunClause -> D
clause c = do
  withVarStyles (Set.toList (funClauseVars c)) do
    let guards =
          case c.guards of
            [] -> mempty
            es@(_:_) -> " when" <+> guardSeq es
    funId c.funid <> parens (listOf pat c.pats) <> guards <+> "->" <+>
      nest 2 (group (line' <> exprs c.body))

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