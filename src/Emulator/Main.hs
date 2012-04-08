{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad.ST (runST)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

import qualified Data.ByteString as B

import Emulator

main :: IO ()
main = do
    progName <- getProgName
    args     <- getArgs
    case args of
        [x] -> do
            bytes <- B.readFile x
            let pretty = runST $ do
                s <- newEmulatorState
                flip runEmulatorM s $ do
                    loadProgram bytes
                    emulate
                    prettify

            putStr pretty

        _   -> do
            putStr $ "Usage: " ++ progName ++ " <executable>"
            exitFailure
