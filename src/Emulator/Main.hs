{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Main where

import Data.Foldable (forM_)

import System.Console.CmdArgs
import qualified Data.ByteString as B

import Emulator
import Emulator.Monad.ST

data Config = Config
    { pretty :: Maybe FilePath
    , binary :: FilePath
    } deriving (Data, Show, Typeable)

config :: Config
config = Config
    { pretty = def &= help "Prettified memory dump" &= typFile
    , binary = def &= argPos 0 &= typFile
    } &= summary "dcpu16-emulator"

main :: IO ()
main = do
    config'  <- cmdArgs config
    program' <- B.readFile (binary config')
    let pretty' = runSTEmulator $ do
            loadProgram program'
            emulate
            prettify

    forM_ (pretty config') $ \path -> writeFile path pretty'
