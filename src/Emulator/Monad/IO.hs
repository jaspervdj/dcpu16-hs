-- | An IO implementation of the emulator. This version supports IO stuff like
-- keyboard events and video.
{-# LANGUAGE GeneralizedNewtypeDeriving, Rank2Types #-}
module Emulator.Monad.IO
    ( IOEmulator
    , runIOEmulator
    ) where

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.ST (RealWorld, stToIO)
import Control.Monad.Trans (lift)

import Emulator.Monad
import Emulator.Video
import Memory (Memory)
import qualified Memory as Memory

newtype IOEmulator a = IOEmulator (ReaderT (Memory RealWorld) IO a)
    deriving (Functor, Monad)

instance MonadEmulator IOEmulator where
    load address = IOEmulator $ do
        mem <- ask
        lift $ stToIO $ Memory.load mem address
    store address word = IOEmulator $ do
        lift $ video address word
        mem <- ask
        lift $ stToIO $ Memory.store mem address word

runIOEmulator :: IOEmulator a -> IO a
runIOEmulator (IOEmulator reader) = do
    mem <- stToIO Memory.new
    runReaderT reader mem
