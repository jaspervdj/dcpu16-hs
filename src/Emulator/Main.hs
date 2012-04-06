{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Control.Monad.ST.Unsafe (unsafeSTToIO)
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
            -- Warning: this code is unsafeSTToIO madness
            --
            -- Start by loading a new emulator state, loading the program,
            -- then step forever
            bytes  <- B.readFile x
            state  <- unsafeSTToIO newEmulatorState
            result <- try $ unsafeSTToIO $ flip runEmulatorM state $ do
                loadProgram bytes
                forever step

            -- Check the result 
            case result of
                Left (ex :: SomeException) -> putStrLn $
                    "Emulator crashed: " ++ show ex
                Right () -> putStrLn $ "Emulator somehow managed to stop."

            -- Pretty dump
            pretty <- unsafeSTToIO $ runEmulatorM prettify state
            putStrLn ""
            putStr pretty

        _   -> do
            putStr $ "Usage: " ++ progName ++ " <executable>"
            exitFailure
