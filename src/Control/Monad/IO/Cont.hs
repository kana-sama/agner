{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Control.Monad.IO.Cont (PromptTag, newPromptTag, prompt, control0) where

import GHC.Prim (PromptTag#, newPromptTag#, prompt#, control0#)
import GHC.IO (IO(..), unIO)

data PromptTag a = MkPromptTag (PromptTag# a)

newPromptTag :: IO (PromptTag a)
newPromptTag = IO (\s -> case newPromptTag# s of (# s, tag #) -> (# s, MkPromptTag tag #))

prompt :: PromptTag a -> IO a -> IO a
prompt (MkPromptTag tag) (IO action) = IO (prompt# tag action)

control0 :: PromptTag a -> ((IO b -> IO a) -> IO a) -> IO b
control0 (MkPromptTag tag) h = IO (control0# tag (\x -> unIO (h (IO . x . unIO))))