module Emulator.Monad
    ( MonadEmulator (..)
    ) where

import Data.Word (Word16)

import Memory (Address)

class (Functor m, Monad m) => MonadEmulator m where
    load  :: Address -> m Word16
    store :: Address -> Word16 -> m ()
