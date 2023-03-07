module Language.Agner.SM where

import Language.Agner.Prelude

import Data.Set qualified as Set
import Data.Map.Strict qualified as Map
import Data.List qualified as List
import Data.Aeson (ToJSON)
import Data.Typeable (eqT)

import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Value (Value)
import Language.Agner.Value qualified as Value
import Language.Agner.Denote qualified as Denote
import Language.Agner.Syntax (BinOp)


data Ex
  = NotEnoughValuesForResult
  | EmptyStack
  | AlreadyHalted
  | PositionIsOutOfProg
  | NoMatch Syntax.Pat Value
  | UnboundVariable Syntax.Var
  | UndefinedFuncton Syntax.FunId
  | DenoteEx Denote.Ex
  | NoFunctionClauseMatching Syntax.FunId [Value]
  | NoEntryPoint
  | BadFunction Value
  | BadArity Syntax.FunId Int
  deriving stock (Show)

instance Exception Ex where
  fromException e
    | Just e <- fromException @Denote.Ex e = Just (DenoteEx e)
    | SomeException (e :: e) <- e, Just Refl <- eqT @e @Ex = Just e
    | otherwise = Nothing

data OnMatchFail = Throw | NextClause Syntax.FunId (Maybe Int)
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

data Instr
  = PUSH_I Integer
  | PUSH_ATOM Syntax.Atom
  | PUSH_FUN Syntax.FunId
  | PUSH_TUPLE Int
  | BINOP Syntax.BinOp
  | DROP
  | DUP
  | CALL{funid :: Syntax.FunId, tailness :: Syntax.CallTailness}
  | DYN_CALL{arity :: Int}
  
  | FUNCTION{funid :: Syntax.FunId, vars :: [Syntax.Var]}
  | CLAUSE{funid :: Syntax.FunId, clauseIndex :: Int, vars :: [Syntax.Var]}
  | FAIL_CLAUSE{funid :: Syntax.FunId, clauseIndex :: Int}
  | FUNCTION_END{funid :: Syntax.FunId}
  | LEAVE Syntax.FunId
  | RET

  | LOAD Syntax.Var
  | MATCH_I Integer OnMatchFail
  | MATCH_VAR Syntax.Var OnMatchFail
  | MATCH_ATOM Syntax.Atom OnMatchFail
  | MATCH_TUPLE Int OnMatchFail

  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

type Prog = [Instr]

compileExpr :: Syntax.Expr -> Prog
compileExpr = \case
  Syntax.Integer i ->
    [PUSH_I i]
  Syntax.Atom a ->
    [PUSH_ATOM a]
  Syntax.Fun f ->
    [PUSH_FUN f]
  Syntax.Tuple es ->
    foldMap compileExpr es ++ [PUSH_TUPLE (length es)]
  Syntax.BinOp a op b ->
    compileExpr a ++ compileExpr b ++ [BINOP op]
  Syntax.Var v ->
    [LOAD v]
  Syntax.Match p e ->
    compileExpr e ++ [DUP] ++ compilePat Throw p
  Syntax.Apply tailness f args -> 
    foldMap compileExpr args ++ [CALL f tailness]
  Syntax.DynApply f args ->
    compileExpr f ++ foldMap compileExpr args ++ [DYN_CALL (length args)]

compilePat :: OnMatchFail -> Syntax.Pat -> Prog
compilePat onMatchFail = \case
  Syntax.PatVar var -> [MATCH_VAR var onMatchFail]
  Syntax.PatInteger i -> [MATCH_I i onMatchFail]
  Syntax.PatWildcard -> [DROP]
  Syntax.PatAtom a -> [MATCH_ATOM a onMatchFail]
  Syntax.PatTuple ps -> [MATCH_TUPLE (length ps) onMatchFail] ++ foldMap (compilePat onMatchFail) ps

compileExprs :: Syntax.Exprs -> Prog
compileExprs exprs = List.intercalate [DROP] [compileExpr e | e <- exprs]

compileFunDecl :: Syntax.FunDecl -> Prog
compileFunDecl funDecl =
  concat
    [ [FUNCTION funDecl.funid (Set.toList (Syntax.funDeclVars funDecl))]
    , foldMap ((uncurry . uncurry) compileFunClause) (zipWithLast (zip funDecl.clauses [0..]))
    , [FAIL_CLAUSE funDecl.funid (length funDecl.clauses)]
    , [FUNCTION_END funDecl.funid]
    ]
  where
    -- красиво
    zipWithLast :: [a] -> [(a, Bool)]
    zipWithLast xs = reverse (zip (reverse xs) ([True] ++ repeat False))

-- args are in original order on stack
compileFunClause :: Syntax.FunClause -> Int -> Bool -> Prog
compileFunClause clause ix isLast =
  concat
    [ [CLAUSE clause.funid ix (Set.toList (Syntax.funClauseVars clause))]
    , foldMap (compilePat (NextClause clause.funid (if isLast then Nothing else Just (ix + 1)))) clause.pats
    , compileExprs clause.body
    , [LEAVE clause.funid]
    , [RET]
    ]

compileModule :: Syntax.Module -> Prog
compileModule mod =
  foldMap compileFunDecl mod.decls

data Frame = MkFrame
  { stack :: [Value]
  , mem :: Map Syntax.Var Value
  , ret :: Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data Cfg = MkCfg
  { pos :: Int
  , prog :: Prog
  , stack :: [Value]
  , mem :: Map Syntax.Var Value
  , frames :: [Frame]
  , halted :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

type M = StateT Cfg IO

push :: Value -> M ()
push v = #stack %= (v:)

pop :: M Value
pop = zoom #stack do
  state \case
    [] -> throw EmptyStack
    v:stack -> (v, stack)

continue :: M ()
continue = #pos += 1

halt :: M ()
halt = #halted .= True

currentInstr :: Cfg -> Instr
currentInstr cfg
  | cfg.pos >= length cfg.prog = throw PositionIsOutOfProg
  | otherwise = cfg.prog !! cfg.pos

skipForNextClause :: M ()
skipForNextClause = do
  instr <- gets currentInstr
  case instr of
    CLAUSE{} -> pure ()
    FAIL_CLAUSE{} -> pure ()
    _ -> continue *> skipForNextClause

type FunEnv = Map Syntax.FunId Int

leave :: M ()
leave = do
  frames <- use #frames
  case frames of
    [] -> pure ()
    frame:frames -> do
      #frames .= frames
      #stack .= frame.stack
      #mem .= frame.mem

instr :: (?funs :: FunEnv) => Instr -> (Cfg -> IO Cfg)
instr (PUSH_I i) = execStateT do
  push (Value.Integer i)
  continue
instr (PUSH_ATOM a) = execStateT do
  push (Value.Atom a)
  continue
instr (PUSH_FUN f) = execStateT do
  push (Value.Fun f)
  continue
instr (PUSH_TUPLE size) = execStateT do
  vs <- reverse <$> replicateM size pop
  push (Value.Tuple vs)
  continue
instr (BINOP op) = execStateT do
  b <- pop
  a <- pop
  push ((Denote.binOp op) a b)
  continue
instr DROP = execStateT do
  void pop
  continue
instr DUP = execStateT do
  a <- pop
  push a
  push a
  continue

instr (CALL f _) | Denote.isBif f = execStateT do
  args <- replicateM f.arity pop
  result <- liftIO do Denote.bif f args
  push result
  continue

-- TODO: do not allocate more frames
instr (CALL f _) = execStateT do
  ret <- use #pos
  args <- replicateM f.arity pop
  stack <- #stack <<.= reverse args
  mem <- #mem <<.= Map.empty
  #frames %= (MkFrame{stack, mem, ret} :)
  case ?funs Map.!? f of
    Nothing -> throw (UndefinedFuncton f)
    Just pos -> #pos .= pos

instr (DYN_CALL arity) = execStateT do
  ret <- use #pos
  args <- replicateM arity pop
  f <- pop >>= \case
    Value.Fun f -> pure f
    value -> throw (BadFunction value)
  when (f.arity /= arity) do
    throw (BadArity f arity)
  stack <- #stack <<.= reverse args
  mem <- #mem <<.= Map.empty
  #frames %= (MkFrame{stack, mem, ret} :)
  case ?funs Map.!? f of
    Nothing -> throw (UndefinedFuncton f)
    Just pos -> #pos .= pos

instr (FUNCTION _ _) = execStateT do
  continue

instr (FUNCTION_END _) = execStateT do
  continue

instr (CLAUSE _ _ _) = execStateT do
  stack <- use #stack
  #frames %= (MkFrame{stack, mem = Map.empty, ret = 0} :)
  continue

instr (FAIL_CLAUSE f _) = execStateT do
  vals <- reverse <$> replicateM f.arity pop
  throw (NoFunctionClauseMatching f vals)

instr (LEAVE _) = execStateT do
  result <- pop
  leave
  push result
  continue

instr RET = execStateT do
  use #frames >>= \case
    [] -> halt
    frame:_ -> do
      result <- pop
      leave
      push result
      #pos .= frame.ret
      continue

instr (LOAD var) = execStateT do
  mem <- use #mem
  case mem Map.!? var of
    Nothing -> throw (UnboundVariable var)
    Just val -> do
      push val
      continue
instr (MATCH_I i onMatchFail) = execStateT do
  value <- pop
  if Value.same (Value.Integer i) value
    then continue
    else case onMatchFail of
           Throw -> throw (NoMatch (Syntax.PatInteger i) value)
           NextClause{} -> leave *> skipForNextClause

instr (MATCH_VAR var onMatchFail) = execStateT do
  val <- pop
  mem <- use #mem
  case mem Map.!? var of
    Nothing -> do
      #mem %= Map.insert var val
      continue
    Just val'
      | Value.same val val' -> continue
      | otherwise ->
        case onMatchFail of
          Throw -> throw (NoMatch (Syntax.PatVar var) val)
          NextClause{} -> leave *> skipForNextClause

instr (MATCH_ATOM a onMatchFail) = execStateT do
  value <- pop
  if Value.same (Value.Atom a) value
    then continue
    else case onMatchFail of
          Throw -> throw (NoMatch (Syntax.PatAtom a) value)
          NextClause{} -> leave *> skipForNextClause

instr (MATCH_TUPLE size onMatchFail) = execStateT do
  values <- pop >>= \case
    Value.Tuple vs -> pure vs
    value -> throw (NoMatch (Syntax.PatTuple (replicate size Syntax.PatWildcard)) value)
  for_ (reverse values) push
  continue

step :: (?funs :: FunEnv) => Cfg -> IO Cfg
step cfg
  | cfg.halted = throw AlreadyHalted
  | otherwise = instr (currentInstr cfg) cfg

buildCfg :: Prog -> Int -> Cfg
buildCfg prog pos =
  MkCfg
    { prog
    , pos
    , stack = []
    , mem = Map.empty
    , frames = []
    , halted = False
    }

data DebugOutput = MkDebugOutput
  { prog :: Prog
  , cfgs :: [Cfg]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- debug :: Int -> Prog -> DebugOutput
-- debug fuel prog =
--   let ?funs = funs
--    in MkDebugOutput{prog, cfgs = go fuel (buildCfg prog entryPoint)}
--   where
--     funs :: FunEnv
--     funs = Map.fromList [(f, i) | (FUNCTION f _, i) <- zip prog [0..]]

--     entryPoint :: Int
--     entryPoint =
--       case funs Map.!? ("main" Syntax.:/ 0) of
--         Nothing -> throw NoEntryPoint
--         Just pos -> pos

--     go 0 cfg = [cfg]
--     go fuel cfg
--       | cfg.halted = [cfg]
--       | otherwise = unsafePerformIO do
--         try @SomeException (evaluate (go (pred fuel) (step cfg))) >>= \case
--           Left e -> do
--             putStrLn (displayException e)
--             pure [cfg]
--           Right cfgs -> pure (cfg:cfgs)

run :: Prog -> IO Value
run prog = do
  cfg <- let ?funs = funs in go (buildCfg prog entryPoint)
  case cfg.stack of
    [result] -> pure result
    _ -> throw NotEnoughValuesForResult
  where
    funs :: FunEnv
    funs = Map.fromList [(f, i) | (FUNCTION f _, i) <- zip prog [0..]]

    entryPoint :: Int
    entryPoint =
      case funs Map.!? ("main" Syntax.:/ 0) of
        Nothing -> throw NoEntryPoint
        Just pos -> pos

    go cfg
      | cfg.halted = pure cfg
      | otherwise = step cfg >>= go