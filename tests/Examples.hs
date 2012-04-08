{-# LANGUAGE Rank2Types #-}
module Examples
    ( tests
    ) where

import Control.Monad.Reader (ask)
import Control.Monad.ST (ST, runST)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))
import qualified Data.ByteString as B

import Assembler
import Emulator
import Memory (Memory)
import qualified Memory as Memory

tests :: Test
tests = testGroup "Examples"
    [ testCase "notch.s" $ example "examples/notch.s" $ \mem -> do
        x <- Memory.load mem $ Memory.register Memory.X
        return $ 0x40 @=? x

    , testCase "sum-squares.s" $ example "examples/sum-squares.s" $ \mem -> do
        x <- Memory.load mem $ Memory.register Memory.X
        return $ sum [n * n | n <- [0 .. 50]] @=? x
    ]

example :: FilePath
        -> (forall s. Memory s -> ST s Assertion)
        -> Assertion
example filePath check = do
    assembleFile filePath "a.out"
    program <- B.readFile "a.out"
    runST $ emu program
  where
    emu program = do
        s   <- newEmulatorState
        mem <- flip runEmulatorM s $ do
            loadProgram program
            emulate
            ask
        check mem
