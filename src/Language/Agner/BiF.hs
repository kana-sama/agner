module Language.Agner.BiF (BiF(..), parse, SpecF(..), Spec, runSpec, spec) where

import Prelude hiding (error)
import Prelude qualified

import Control.Monad.Free
import Control.Concurrent (threadDelay)

import Language.Agner.Prelude
import Language.Agner.Value (Value, PID)
import Language.Agner.Value qualified as Value
import Language.Agner.Syntax (FunId)

data BiF
  = BiF__agner__print__1
  | BiF__timer__sleep__1
  | BiF__erlang__error__1
  | BiF__erlang__spawn__1
  | BiF__erlang__self__0
  deriving stock Show

parse :: FunId -> Maybe BiF
parse = \case
  "self/0" -> Just BiF__erlang__self__0
  "erlang:self/0" -> Just BiF__erlang__self__0
  "spawn/1" -> Just BiF__erlang__spawn__1
  "erlang:spawn/1" -> Just BiF__erlang__spawn__1
  "agner:print/1" -> Just BiF__agner__print__1
  "error/1" -> Just BiF__erlang__error__1
  "erlang:error/1" -> Just BiF__erlang__error__1
  "timer:sleep/1" -> Just BiF__timer__sleep__1
  _ -> Nothing


data SpecF next
  = forall a. RunIO (IO a) (a -> next)
  | Spawn FunId (PID -> next)
  | Yield next
  | Self (PID -> next)
  | Error Value

instance Functor SpecF where
  fmap f = \case
    RunIO io k -> RunIO io (f . k)
    Spawn action k -> Spawn action (f . k)
    Yield k -> Yield (f k)
    Self k -> Self (f . k)
    Error v -> Error v

type Spec = Free SpecF

runIO :: IO a -> Spec a
runIO io = liftF (RunIO io id)

spawn :: FunId -> Spec PID
spawn action = liftF (Spawn action id)

yield :: Spec ()
yield = liftF (Yield ())

self :: Spec PID
self = liftF (Self id)

error :: Value -> Spec a
error v = liftF (Error v)


spec :: BiF -> ([Value] -> Spec Value)
spec BiF__agner__print__1 [value] = do
  yield
  runIO do putStrLn (Value.encode value)
  pure (Value.Atom "ok")
spec BiF__timer__sleep__1 [Value.Integer duration] = do
  yield
  runIO do threadDelay (fromInteger duration * 1000)
  pure (Value.Atom "ok")
spec BiF__timer__sleep__1 [Value.Atom "infinity"] = do
  yield
  forever do
    runIO do threadDelay (1000 * 1000)
spec BiF__erlang__error__1 [value] = do
  yield
  error value
spec BiF__erlang__spawn__1 [Value.Fun funid] = do
  yield
  Value.PID <$> spawn funid
spec BiF__erlang__self__0 [] = do
  yield
  Value.PID <$> self
spec bif args = do
  Prelude.error ("Invalid bif call: " ++ show bif ++ " with " ++ show args)

runSpec :: Monad m => (forall a. SpecF a -> m a) -> (forall a. Spec a -> m a)
runSpec = foldFree
