-- | A pure implementation of the emulator
{-# LANGUAGE GeneralizedNewtypeDeriving, Rank2Types #-}
module Emulator.Monad.ST
    ( STEmulator
    , runSTEmulator
    ) where

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans (lift)

import Emulator.Monad
import Memory (Memory)
import qualified Memory as Memory

newtype STEmulator s a = STEmulator (ReaderT (Memory s) (ST s) a)
    deriving (Functor, Monad)

instance MonadEmulator (STEmulator s) where
    load address = STEmulator $ do
        mem <- ask
        lift $ Memory.load mem address
    store address word = STEmulator $ do
        mem <- ask
        lift $ Memory.store mem address word

runSTEmulator :: (forall s. STEmulator s a) -> a
runSTEmulator emu =
    -- If you wonder why this isn't defined as `runST . run`... type magic.
    let x = run emu
    in runST x
  where
    run :: STEmulator s a -> ST s a
    run (STEmulator reader) = do
        mem <- Memory.new
        runReaderT reader mem
