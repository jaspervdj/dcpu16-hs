{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Main where

import Control.Monad.Trans (liftIO)
import Data.Foldable (forM_)
import Prelude hiding (log)

import System.Console.CmdArgs
import qualified Data.ByteString as B

import Emulator
import Emulator.Monad.IO
import qualified Emulator.Log as Log

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
    pretty'  <- runIOEmulator $ do
            loadProgram program'
            emulateWith (\i i' -> Log.state i i' >>= liftIO . putStrLn)
            Log.prettify

    forM_ (pretty config') $ \path -> writeFile path pretty'
