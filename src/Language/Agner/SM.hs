module Language.Agner.SM where

import Language.Agner.Prelude

import Data.Set qualified as Set
import Data.Map.Strict qualified as Map
import Data.List qualified as List
import Data.Aeson (ToJSON)
import Data.Typeable (eqT)
import Data.IORef (IORef, newIORef, modifyIORef')

import Language.Agner.Syntax (FunId)
import Language.Agner.Syntax qualified as Syntax
import Language.Agner.Value (Value, PID)
import Language.Agner.Value qualified as Value
import Language.Agner.Denote qualified as Denote
import Language.Agner.Syntax (BinOp)
import Language.Agner.BiF qualified as BiF


-- data Ex
--   = EmptyStack
--   | AlreadyHalted
--   | PositionIsOutOfProg
--   | NoMatch Syntax.Pat Value
--   | UnboundVariable Syntax.Var
--   | UndefinedFuncton FunId
--   | DenoteEx Denote.Ex
--   | NoFunctionClauseMatching FunId [Value]
--   | NoEntryPoint
--   | BadFunction Value
--   | BadArity FunId Int
--   deriving stock (Show)

-- instance Exception Ex where
--   fromException e
--     | Just e <- fromException @Denote.Ex e = Just (DenoteEx e)
--     | SomeException (e :: e) <- e, Just Refl <- eqT @e @Ex = Just e
--     | otherwise = Nothing

data OnMatchFail = Throw | NextClause FunId (Maybe Int)
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

data Instr
  = PUSH_I Integer
  | PUSH_ATOM Syntax.Atom
  | PUSH_FUN FunId
  | PUSH_TUPLE Int
  | PUSH_NIL
  | PUSH_CONS
  | BINOP Syntax.BinOp
  | DROP
  | DUP
  | CALL{funid :: FunId, tailness :: Syntax.CallTailness}
  | DYN_CALL{arity :: Int}
  
  | FUNCTION{funid :: FunId, vars :: [Syntax.Var]}
  | YIELD
  | CLAUSE{funid :: FunId, clauseIndex :: Int, vars :: [Syntax.Var]}
  | FAIL_CLAUSE{funid :: FunId, clauseIndex :: Int}
  | FUNCTION_END{funid :: FunId}
  | LEAVE FunId
  | RET

  | LOAD Syntax.Var
  | MATCH_I Integer OnMatchFail
  | MATCH_VAR Syntax.Var OnMatchFail
  | MATCH_ATOM Syntax.Atom OnMatchFail
  | MATCH_TUPLE Int OnMatchFail
  | MATCH_NIL OnMatchFail
  | MATCH_CONS OnMatchFail

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
  Syntax.Nil ->
    [PUSH_NIL]
  Syntax.Cons a b ->
    compileExpr a ++ compileExpr b ++ [PUSH_CONS]
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
  Syntax.PatNil -> [MATCH_NIL onMatchFail]
  Syntax.PatCons a b -> [MATCH_CONS onMatchFail] ++ compilePat onMatchFail a ++ compilePat onMatchFail b

compileExprs :: Syntax.Exprs -> Prog
compileExprs exprs = List.intercalate [DROP] [compileExpr e | e <- exprs]

compileFunDecl :: Syntax.FunDecl -> Prog
compileFunDecl funDecl =
  concat
    [ [FUNCTION funDecl.funid (Set.toList (Syntax.funDeclVars funDecl))]
    , [YIELD]
    , foldMap ((uncurry . uncurry) compileFunClause) (zipWithLast (zip funDecl.clauses [0..]))
    , [FAIL_CLAUSE funDecl.funid (length funDecl.clauses)]
    , [FUNCTION_END funDecl.funid]
    ]
  where
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

-- _FUEL = 10

-- data Frame = MkFrame
--   { stack :: [Value]
--   , mem :: Map Syntax.Var Value
--   , ret :: Int
--   }
--   deriving stock (Generic)
--   deriving anyclass (ToJSON)

-- data Process = MkProcess
--   { pid :: PID
--   , pos :: Int
--   , stack :: [Value]
--   , mem :: Map Syntax.Var Value
--   , frames :: [Frame]
--   }
--   deriving stock (Generic)
--   deriving anyclass (ToJSON)

-- data Cfg = MkCfg
--   { current :: Process
--   , prog :: Prog
--   , halted :: Bool
--   , queue :: [Process]
--   , gen :: Integer
--   , fuel :: Int
--   }
--   deriving stock (Generic)
--   deriving anyclass (ToJSON)

-- type M = StateT Cfg IO

-- push :: Value -> M ()
-- push v = #current . #stack %= (v:)

-- pop :: M Value
-- pop = zoom (#current . #stack) do
--   state \case
--     [] -> throw EmptyStack
--     v:stack -> (v, stack)

-- continue :: M ()
-- continue = #current . #pos += 1

-- self :: M PID
-- self = use (#current . #pid)

-- yield :: M ()
-- yield = do
--   fuel <- #fuel <<-= 1
--   when (fuel <= 0) do
--     process <- use #current
--     #queue <>= [process]
--     switch

-- spawn :: (?funs :: FunEnv) => FunId -> M PID
-- spawn funid = do
--   pid <- Value.MkPID <$> (#gen <<+= 1)
--   process <- mkProcess pid
--   #queue <>= [process]
--   pure pid
--   where
--     mkProcess pid = do
--       pos <- case ?funs Map.!? funid of
--         Nothing -> throw (UndefinedFuncton funid)
--         Just pos -> pure pos
--       pure MkProcess
--         { pid, pos
--         , stack = []
--         , mem = Map.empty
--         , frames = []
--         }

-- switch :: M ()
-- switch =
--   use #queue >>= \case
--     [] -> #halted .= True
--     process:queue -> do
--       #fuel .= _FUEL
--       #queue .= queue
--       #current .= process

-- currentInstr :: Cfg -> Instr
-- currentInstr cfg
--   | cfg.current.pos >= length cfg.prog = throw PositionIsOutOfProg
--   | otherwise = cfg.prog !! cfg.current.pos

-- skipForNextClause :: M ()
-- skipForNextClause = do
--   instr <- gets currentInstr
--   case instr of
--     CLAUSE{} -> pure ()
--     FAIL_CLAUSE{} -> pure ()
--     _ -> continue *> skipForNextClause

-- type FunEnv = Map FunId Int

-- leave :: M ()
-- leave = do
--   frames <- use (#current . #frames)
--   case frames of
--     [] -> pure ()
--     frame:frames -> do
--       #current . #frames .= frames
--       #current . #stack .= frame.stack
--       #current . #mem .= frame.mem

-- instr :: (?funs :: FunEnv) => Instr -> (Cfg -> IO Cfg)
-- instr (PUSH_I i) = execStateT do
--   push (Value.Integer i)
--   continue
-- instr (PUSH_ATOM a) = execStateT do
--   push (Value.Atom a)
--   continue
-- instr (PUSH_FUN f) = execStateT do
--   push (Value.Fun f)
--   continue
-- instr (PUSH_TUPLE size) = execStateT do
--   vs <- reverse <$> replicateM size pop
--   push (Value.Tuple vs)
--   continue
-- instr PUSH_NIL = execStateT do
--   push Value.Nil
--   continue
-- instr PUSH_CONS = execStateT do
--   b <- pop
--   a <- pop
--   push (Value.Cons a b)
--   continue
-- instr (BINOP op) = execStateT do
--   b <- pop
--   a <- pop
--   push ((Denote.binOp op) a b)
--   continue
-- instr DROP = execStateT do
--   void pop
--   continue
-- instr DUP = execStateT do
--   a <- pop
--   push a
--   push a
--   continue

-- instr (CALL f _) | Just b <- BiF.parse f = execStateT do
--   args <- replicateM f.arity pop
--   result <- BiF.runSpec alg (BiF.spec b args)
--   push result
--   continue
--   where
--     alg :: BiF.SpecF a -> StateT Cfg IO a
--     alg = \case
--       BiF.RunIO io k -> k <$> liftIO io
--       BiF.Spawn funid k -> k <$> spawn funid
--       BiF.Yield k -> k <$ yield
--       BiF.Self k -> k <$> self
--       BiF.Error e -> throw (DenoteEx (Denote.Custom e))

-- -- TODO: do not allocate more frames
-- instr (CALL f _) = execStateT do
--   ret <- use (#current . #pos)
--   args <- replicateM f.arity pop
--   stack <- #current . #stack <<.= reverse args
--   mem <- #current . #mem <<.= Map.empty
--   #current . #frames %= (MkFrame{stack, mem, ret} :)
--   case ?funs Map.!? f of
--     Nothing -> throw (UndefinedFuncton f)
--     Just pos -> #current . #pos .= pos

-- instr (DYN_CALL arity) = execStateT do
--   ret <- use (#current . #pos)
--   args <- replicateM arity pop
--   f <- pop >>= \case
--     Value.Fun f -> pure f
--     value -> throw (BadFunction value)
--   when (f.arity /= arity) do
--     throw (BadArity f arity)
--   stack <- #current . #stack <<.= reverse args
--   mem <- #current . #mem <<.= Map.empty
--   #current . #frames %= (MkFrame{stack, mem, ret} :)
--   case ?funs Map.!? f of
--     Nothing -> throw (UndefinedFuncton f)
--     Just pos -> #current . #pos .= pos

-- instr (FUNCTION _ _) = execStateT do
--   continue

-- instr YIELD = execStateT do
--   yield
--   continue

-- instr (FUNCTION_END _) = execStateT do
--   continue

-- instr (CLAUSE _ _ _) = execStateT do
--   stack <- use (#current . #stack)
--   #current . #frames %= (MkFrame{stack, mem = Map.empty, ret = 0} :)
--   continue

-- instr (FAIL_CLAUSE f _) = execStateT do
--   vals <- reverse <$> replicateM f.arity pop
--   throw (NoFunctionClauseMatching f vals)

-- instr (LEAVE _) = execStateT do
--   result <- pop
--   leave
--   push result
--   continue

-- instr RET = execStateT do
--   use (#current . #frames) >>= \case
--     [] -> switch
--     frame:_ -> do
--       result <- pop
--       leave
--       push result
--       #current . #pos .= frame.ret
--       continue

-- instr (LOAD var) = execStateT do
--   mem <- use (#current . #mem)
--   case mem Map.!? var of
--     Nothing -> throw (UnboundVariable var)
--     Just val -> do
--       push val
--       continue

-- instr (MATCH_I i onMatchFail) = execStateT do
--   value <- pop
--   if Value.same (Value.Integer i) value
--     then continue
--     else case onMatchFail of
--            Throw -> throw (NoMatch (Syntax.PatInteger i) value)
--            NextClause{} -> leave *> skipForNextClause

-- instr (MATCH_VAR var onMatchFail) = execStateT do
--   val <- pop
--   mem <- use (#current . #mem)
--   case mem Map.!? var of
--     Nothing -> do
--       #current . #mem %= Map.insert var val
--       continue
--     Just val'
--       | Value.same val val' -> continue
--       | otherwise ->
--         case onMatchFail of
--           Throw -> throw (NoMatch (Syntax.PatVar var) val)
--           NextClause{} -> leave *> skipForNextClause

-- instr (MATCH_ATOM a onMatchFail) = execStateT do
--   value <- pop
--   if Value.same (Value.Atom a) value
--     then continue
--     else case onMatchFail of
--           Throw -> throw (NoMatch (Syntax.PatAtom a) value)
--           NextClause{} -> leave *> skipForNextClause

-- instr (MATCH_TUPLE size onMatchFail) = execStateT do
--   values <- pop >>= \case
--     Value.Tuple vs -> pure vs
--     value -> throw (NoMatch (Syntax.PatTuple (replicate size Syntax.PatWildcard)) value)
--   for_ (reverse values) push
--   continue

-- instr (MATCH_NIL onMatchFail) = execStateT do
--   value <- pop
--   if Value.same Value.Nil value
--     then continue
--     else case onMatchFail of
--           Throw -> throw (NoMatch Syntax.PatNil value)
--           NextClause{} -> leave *> skipForNextClause

-- instr (MATCH_CONS onMatchFail) = execStateT do
--   pop >>= \case
--     Value.Cons a b -> do
--       push b
--       push a
--       continue
--     value ->
--       case onMatchFail of
--         Throw -> throw (NoMatch (Syntax.PatCons Syntax.PatWildcard Syntax.PatWildcard) value)
--         NextClause{} -> leave *> skipForNextClause

-- step :: (?funs :: FunEnv) => Cfg -> IO Cfg
-- step cfg
--   | cfg.halted = throw AlreadyHalted
--   | otherwise = instr (currentInstr cfg) cfg

-- buildCfg :: Prog -> Int -> Cfg
-- buildCfg prog entry =
--   MkCfg
--     { prog
--     , current = MkProcess
--         { pid = Value.MkPID 0
--         , pos = entry
--         , stack = []
--         , mem = Map.empty
--         , frames = []
--         }
--     , halted = False
--     , gen = 1
--     , fuel = _FUEL
--     , queue = []
--     }

-- run :: Prog -> IO ()
-- run = runWithDebug Nothing

-- runWithDebug :: Maybe (IORef [Cfg]) -> Prog -> IO ()
-- runWithDebug debug prog = do
--   let ?funs = funs
--   go (buildCfg prog entryPoint)
--   pure ()
--   where
--     funs :: FunEnv
--     funs = Map.fromList [(f, i) | (FUNCTION f _, i) <- zip prog [0..]]

--     entryPoint :: Int
--     entryPoint =
--       case funs Map.!? ("main" Syntax.:/ 0) of
--         Nothing -> throw NoEntryPoint
--         Just pos -> pos

--     pushDebug cfg =
--       case debug of
--         Nothing -> pure ()
--         Just ref -> modifyIORef' ref (cfg :)

--     go :: (?funs :: FunEnv) => Cfg -> IO Cfg
--     go cfg
--       | cfg.halted = do
--         pushDebug cfg
--         pure cfg
--       | otherwise = do
--         pushDebug cfg
--         step cfg >>= go