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

type Arity = Int

data FunId = MkFunId { ns :: ModuleName, name :: FunName, arity :: Arity }
  deriving stock (Eq, Ord, Data)

pattern MkUnresolvedFunId{name, arity} = MkFunId{ns = "", name, arity}

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

newtype GuardExpr = MkGuardExpr{getExpr :: Expr}
  deriving stock (Show, Data)

type Guard = [GuardExpr]
type GuardSeq = [Guard]

data CaseBranch = MkCaseBranch
  { pat    :: Pat
  , guards :: GuardSeq
  , body   :: Exprs
  }
  deriving stock (Show, Data, Generic)

data CatchClass
  = CatchClassDefault
  | CatchClassAtom Atom
  | CatchClassVar Var
  deriving stock (Show, Data, Generic)

data CatchBranch = MkCatchBranch
  { class_ :: CatchClass
  , branch :: CaseBranch
  }
  deriving stock (Show, Data, Generic)

data IfBranch = MkIfBranch
  { guards :: GuardSeq
  , body :: Exprs
  }
  deriving stock (Show, Data)

data MapElemBind lhs rhs
  = (:=>) lhs rhs
  | (::=) lhs rhs
  deriving stock (Show, Data)

data CompQualifier
  = CompListGenerator Pat Expr
  | CompMapGenerator Pat Pat Expr
  | CompFilter Expr
  deriving stock (Show, Data)

data MaybeExpr
  = MaybeExpr Expr
  | MaybeBind Pat Expr
  deriving stock (Show, Data)

data ReceiveTimeout after = MkReceiveTimeout
  { timeout :: Expr
  , after :: after
  }
  deriving stock (Show, Data)
  deriving stock (Functor, Foldable, Traversable)

data Expr
  = Integer Integer
  | Atom Atom
  | Tuple [Expr]
  | Nil
  | Cons Expr Expr

  | Record RecordName [(RecordField, Expr)]
  | RecordGet Expr RecordName RecordField
  | RecordUpdate Expr RecordName [(RecordField, Expr)]
  | RecordSelector RecordName RecordField

  | Map [MapElemBind Expr Expr]
  | MapUpdate Expr [MapElemBind Expr Expr]

  | ListComp Expr [CompQualifier]
  | MapComp Expr Expr [CompQualifier]

  | Var Var
  | Fun{funid :: FunId}
  | FunL{clauses :: [Clause]}

  | BinOp BinOp Expr Expr
  | UnOp UnOp Expr

  | Match Pat Expr

  | Apply FunId [Expr]
  | TailApply FunId [Expr]
  | DynApply Expr [Expr]

  | If [IfBranch]
  | Case Expr [CaseBranch]
  | Receive [CaseBranch] (Maybe (ReceiveTimeout Exprs))

  | AndAlso Expr Expr
  | OrElse Expr Expr

  | Send Expr Expr

  | Begin Exprs
  | Maybe [MaybeExpr] [CaseBranch]

  | Catch Expr
  | Try{exprs :: Exprs, branches :: [CatchBranch], after :: Exprs}
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
  | RecordDecl{recordName :: RecordName, recordFields :: [RecordField]}
  | Primitive{funid :: FunId}
  | ImportDecl{moduleName :: ModuleName, names :: [FunId]}
  | ExportDecl{names :: [FunId]}
  deriving stock (Show, Data, Generic)

data Module = MkModule
  { name :: ModuleName
  , decls :: [Decl]
  }
  deriving stock (Show, Data, Generic)


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
