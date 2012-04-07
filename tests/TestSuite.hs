{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Control.Monad.Reader (ask)
import Control.Monad.ST.Unsafe (unsafeSTToIO)
import Control.Monad.Trans (lift)

import qualified Data.ByteString as B

import Assembler
import Emulator
import qualified Memory as Memory

main :: IO ()
main = do
    assembleFile "examples/notch.s" "a.out"
    program <- B.readFile "a.out"
    state   <- unsafeSTToIO newEmulatorState
    (_ :: Either SomeException ()) <- try $
        unsafeSTToIO $ flip runEmulatorM state $ do
            loadProgram program
            forever step

    x <- unsafeSTToIO $ flip runEmulatorM state $ do
        mem <- ask
        lift $ Memory.load mem $ Memory.register Memory.X

    print $ x == 0x40
