{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs

import Assembler

data Config = Config
    { output :: FilePath
    , source :: FilePath
    } deriving (Data, Show, Typeable)

config :: Config
config = Config
    { output = "a.out" &= help "Output file" &= typFile
    , source = def     &= argPos 0 &= typFile
    } &= summary "dcpu16-assembler"

main :: IO ()
main = do
    config' <- cmdArgs config
    assembleFile (source config') (output config')
