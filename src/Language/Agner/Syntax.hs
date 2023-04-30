module Language.Agner.Syntax where

import Language.Agner.Prelude

import Data.Set qualified as Set
import Data.List qualified as List
import Data.Char qualified as Char
import Data.Generics.Uniplate.Data (universeBi)

newtype Var         = MkVar         {getString :: String} deriving stock (Data) deriving newtype (Show, Eq, Ord, IsString)
newtype Atom        = MkAtom        {getString :: String} deriving stock (Data) deriving newtype (Show, Eq, Ord, IsString)
newtype FunName     = MkFunName     {getString :: String} deriving stock (Data) deriving newtype (Show, Eq, Ord, IsString)
newtype ModuleName  = MkModuleName  {getString :: String} deriving stock (Data) deriving newtype (Show, Eq, Ord, IsString)
newtype RecordName  = MkRecordName  {getString :: String} deriving stock (Data) deriving newtype (Show, Eq, Ord, IsString)
newtype RecordField = MkRecordField {getString :: String} deriving stock (Data) deriving newtype (Show, Eq, Ord, IsString)

data FunId = MkFunId { ns :: ModuleName, name :: FunName, arity :: Int }
  deriving stock (Eq, Ord, Data)

data BinOp
  = Plus | Minus | Times | Div | Rem
  | BAnd | BOr | BXor | BSL | BSR
  | And | Or | Xor
  | Plus_Plus | Minus_Minus
  | Eq_Eq | Slash_Eq | Eq_Less | Less | Greater_Eq | Greater | Eq_Colon_Eq | Eq_Slash_Eq
  deriving stock (Show, Eq, Ord, Data)

data UnOp = Plus' | Minus' | BNot | Not
  deriving stock (Show, Eq, Ord, Data)

data Operator = Unary UnOp | Binary BinOp
  deriving stock (Show, Eq, Ord, Data)

type GuardSeq = [GuardExp]
type GuardExp = [Expr]

data CaseBranch = CaseBranch
  { pat    :: Pat
  , guards :: GuardSeq
  , body   :: Exprs
  }
  deriving stock (Show, Data)
  
data MapElemBind
  = (:=>) Expr Expr
  | (::=) Expr Expr
  deriving stock (Show, Data)

data ListCompQualifier
  = ListCompGenerator Pat Expr
  | ListCompFilter Expr
  deriving stock (Show, Data)

data Expr
  = Integer Integer
  | Atom Atom
  | Tuple [Expr]
  | Nil
  | Cons Expr Expr
  
  | Record RecordName [(RecordField, Expr)]
  | RecordGet Expr RecordName RecordField
  | RecordUpdate Expr RecordName [(RecordField, Expr)]

  | Map [MapElemBind]
  | MapUpdate Expr [MapElemBind]

  | ListComp Expr [ListCompQualifier]

  | Var Var
  | Fun{funid :: FunId}
  | FunL{clauses :: [Clause]}

  | BinOp BinOp Expr Expr
  | UnOp UnOp Expr

  | Match Pat Expr

  | Apply FunId [Expr]
  | TailApply FunId [Expr]
  | DynApply Expr [Expr]

  | Case Expr [CaseBranch]
  | Receive [CaseBranch]

  | AndAlso Expr Expr
  | OrElse Expr Expr

  | Send Expr Expr

  | Begin Exprs
  deriving stock (Show, Data)

type Exprs = [Expr]

data Pat
  = PatVar Var
  | PatWildcard
  | PatInteger Integer
  | PatAtom Atom
  | PatTuple [Pat]
  | PatNil
  | PatCons Pat Pat
  | PatRecord RecordName [(RecordField, Pat)]
  | PatMatch Pat Pat
  deriving stock (Show, Data)

data Clause = MkClause
  { pats :: [Pat]
  , guards :: GuardSeq
  , body :: Exprs
  }
  deriving stock (Show, Data)

data Decl
  = FunDecl{funid :: FunId, clauses :: [Clause]}
  | Primitive{funid :: FunId}
  | RecordDecl{recordName :: RecordName, recordFields :: [RecordField]}
  deriving stock (Show, Data)

data Module = MkModule
  { name :: ModuleName
  , decls :: [Decl]
  }
  deriving stock (Show, Data)


-- utils

binOpName :: BinOp -> String
binOpName = map Char.toLower . show

unOpName :: UnOp -> String
unOpName = dropTick . map Char.toLower . show where
  dropTick s
    | '\'' <- last s = init s
    | otherwise = s

instance IsString FunId where
  fromString src0 =
    let (ns,   ':':src1) = List.break (== ':') src0
        (name, '/':src2) = List.break (== '/') src1
        arity = read src2
     in MkFunId (MkModuleName ns) (MkFunName name) arity

prettyFunId, prettyFunIdNoArity :: FunId -> String
prettyFunId f = prettyFunIdNoArity f ++ "/" ++ show f.arity
prettyFunIdNoArity f = f.ns.getString ++ ":" ++ f.name.getString

instance Show FunId where
  show f = show (prettyFunId f)

allVars :: Data a => a -> Set Var
allVars = Set.fromList . universeBi
