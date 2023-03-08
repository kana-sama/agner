module Language.Agner.Prelude
  ( nonEmptyError
  , module X
  ) where

import Data.Map.Strict as X (Map)
import Data.Set as X (Set)
import Data.Foldable as X (foldlM, for_, traverse_, toList)
import Data.Void as X (Void)
import Data.Generics.Labels as X ()
import Data.String as X (IsString(..))
import Data.Traversable as X (for)

import Debug.Trace as X

import Control.Monad as X (void)
import Control.Exception as X (Exception (..), try, throw, SomeException(..), evaluate)
import Control.Monad.State.Strict as X
import Control.Monad.Except as X

import Control.Lens as X hiding (pattern List)
import GHC.Generics as X (Generic)

nonEmptyError :: String -> a
nonEmptyError source = error (source ++ ": empty non-empty")
