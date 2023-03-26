module Language.Agner.BiF (BiF(..), toFunId, parse, SpecF(..), Spec, runSpec, spec) where

import Prelude hiding (error)
import Prelude qualified

import Data.Char qualified as Char

import Control.Monad.Free
import Control.Concurrent (threadDelay)

import Language.Agner.Prelude
import Language.Agner.Value (Value, PID)
import Language.Agner.Value qualified as Value
import Language.Agner.Syntax (FunId)

data BiF
  = BiF__agner__print__1
  | BiF__agner__println__1
  | BiF__agner__put_char__1
  | BiF__agner__put_str__1
  | BiF__timer__sleep__1
  | BiF__erlang__error__1
  | BiF__erlang__spawn__1
  | BiF__erlang__self__0
  | BiF__erlang__send__2
  deriving stock Show

toFunId :: BiF -> FunId
toFunId = \case
  BiF__agner__print__1 -> "agner:print/1"
  BiF__agner__println__1 -> "agner:println/1"
  BiF__agner__put_char__1 -> "agner:put_char/1"
  BiF__agner__put_str__1 -> "agner:put_str/1"
  BiF__timer__sleep__1 -> "timer:sleep/1"
  BiF__erlang__error__1 -> "erlang:error/1"
  BiF__erlang__spawn__1 -> "erlang:spawn/1"
  BiF__erlang__self__0 -> "erlang:self/0"
  BiF__erlang__send__2 -> "erlang:send/2"

parse :: FunId -> Maybe BiF
parse = \case
  "self/0"  -> Just BiF__erlang__self__0
  "spawn/1" -> Just BiF__erlang__spawn__1
  "error/1" -> Just BiF__erlang__error__1

  "erlang:self/0"  -> Just BiF__erlang__self__0
  "erlang:spawn/1" -> Just BiF__erlang__spawn__1
  "erlang:error/1" -> Just BiF__erlang__error__1
  "erlang:send/2"  -> Just BiF__erlang__send__2
  
  "agner:print/1" -> Just BiF__agner__print__1
  "agner:println/1" -> Just BiF__agner__println__1
  "agner:put_char/1" -> Just BiF__agner__put_char__1
  "agner:put_str/1" -> Just BiF__agner__put_str__1

  "timer:sleep/1" -> Just BiF__timer__sleep__1

  _ -> Nothing


data SpecF next
  = forall a. RunIO (IO a) (a -> next)
  | Spawn FunId (PID -> next)
  | Yield FunId next
  | Self (PID -> next)
  | Send PID Value next
  | Error Value

instance Functor SpecF where
  fmap f = \case
    RunIO io k -> RunIO io (f . k)
    Spawn action k -> Spawn action (f . k)
    Yield funid k -> Yield funid (f k)
    Self k -> Self (f . k)
    Send pid value k -> Send pid value (f k)
    Error v -> Error v

type Spec = Free SpecF

runIO :: IO a -> Spec a
runIO io = liftF (RunIO io id)

spawn :: FunId -> Spec PID
spawn action = liftF (Spawn action id)

yield :: FunId -> Spec ()
yield f = liftF (Yield f ())

self :: Spec PID
self = liftF (Self id)

send :: PID -> Value -> Spec ()
send pid value = liftF (Send pid value ())

error :: Value -> Spec a
error v = liftF (Error v)

bifError bif args = Prelude.error ("Invalid bif call: " ++ show bif ++ " with " ++ show args)

spec :: BiF -> ([Value] -> Spec Value)

spec BiF__agner__print__1 [value] = do
  yield (toFunId BiF__agner__print__1)
  runIO do putStr (Value.encode value)
  pure (Value.Atom "ok")
spec BiF__agner__print__1 args = bifError BiF__agner__print__1 args

spec BiF__agner__println__1 [value] = do
  yield (toFunId BiF__agner__println__1)
  runIO do putStrLn (Value.encode value)
  pure (Value.Atom "ok")
spec BiF__agner__println__1 args = bifError BiF__agner__println__1 args

spec BiF__agner__put_char__1 [Value.Integer i] = do
  yield (toFunId BiF__agner__put_char__1)
  runIO do putStr [Char.chr (fromInteger i)]
  pure (Value.Atom "ok")
spec BiF__agner__put_char__1 args = bifError BiF__agner__put_char__1 args

spec BiF__agner__put_str__1 [Value.List vs] | all Value.isPrintableLatin1 vs = do
  yield (toFunId BiF__agner__put_str__1)
  runIO do putStr [Char.chr (fromInteger i) | Value.Integer i <- vs]
  pure (Value.Atom "ok")
spec BiF__agner__put_str__1 args = bifError BiF__agner__put_str__1 args

spec BiF__timer__sleep__1 [Value.Integer duration] = do
  yield (toFunId BiF__timer__sleep__1)
  runIO do threadDelay (fromInteger duration * 1000)
  pure (Value.Atom "ok")
spec BiF__timer__sleep__1 [Value.Atom "infinity"] = do
  yield (toFunId BiF__timer__sleep__1)
  forever do
    runIO do threadDelay (1000 * 1000)
spec BiF__timer__sleep__1 args = bifError BiF__timer__sleep__1 args

spec BiF__erlang__error__1 [value] = do
  yield (toFunId BiF__erlang__error__1)
  error value
spec BiF__erlang__error__1 args = bifError BiF__erlang__error__1 args

spec BiF__erlang__spawn__1 [Value.Fun funid] = do
  yield (toFunId BiF__erlang__spawn__1)
  Value.PID <$> spawn funid
spec BiF__erlang__spawn__1 args = bifError BiF__erlang__spawn__1 args

spec BiF__erlang__self__0 [] = do
  yield (toFunId BiF__erlang__self__0)
  Value.PID <$> self
spec BiF__erlang__self__0 args = bifError BiF__erlang__self__0 args

spec BiF__erlang__send__2 [Value.PID pid, msg] = do
  yield (toFunId BiF__erlang__send__2)
  send pid msg
  pure msg
spec BiF__erlang__send__2 args = bifError BiF__erlang__send__2 args

runSpec :: Monad m => (forall a. SpecF a -> m a) -> (forall a. Spec a -> m a)
runSpec = foldFree
