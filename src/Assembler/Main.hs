module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

import Assembler

main :: IO ()
main = do
    progName <- getProgName
    args     <- getArgs
    case args of
        [x] -> assembleFile x "a.out"
        _   -> do
            putStr $ "Usage: " ++ progName ++ " <assembler file>"
            exitFailure
