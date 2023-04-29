module Language.Agner.Prelude (module X, whileM) where

import Data.Data as X (Data)
import Data.Map.Strict as X (Map)
import Data.Set as X (Set)
import Data.Foldable as X (foldlM, for_, traverse_, toList)
import Data.Void as X (Void)
import Data.Generics.Labels as X ()
import Data.String as X (IsString(..))
import Data.Traversable as X (for)
import Data.Maybe as X (fromMaybe, fromJust)
import Data.Functor as X (($>))
import Data.Coerce as X (coerce)

import Data.Foldable.WithIndex as X (ifor_)
import Data.Traversable.WithIndex as X (ifor)

import Debug.Trace as X

import Control.Monad as X (void, forever, (>=>), when, unless, replicateM, replicateM_, guard)
import Control.Exception as X (Exception (..), try, throw, SomeException(..), evaluate)
import Control.Monad.State.Strict as X
import Control.Monad.Except as X
import Control.Monad.Reader as X
import Control.Monad.Writer as X
import Control.Monad.Fix as X

import Control.Lens as X hiding (pattern List, Context, argument)
import GHC.Generics as X (Generic)

whileM :: Monad m => m Bool -> m () -> m ()
whileM cond body =
  let loop = cond >>= \case
        True -> do body; loop
        False -> pure ()
   in loop
