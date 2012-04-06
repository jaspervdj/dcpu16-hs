module Main where

import Control.Monad (replicateM_)
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
            putStr $ runEmulatorM $ do
                loadProgram bytes
                replicateM_ 10000 step
                prettify
        _   -> do
            putStr $ "Usage: " ++ progName ++ " <executable>"
            exitFailure
