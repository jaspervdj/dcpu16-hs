{-# LANGUAGE Rank2Types #-}
module Examples
    ( tests
    ) where

import Control.Monad (forM)
import Control.Monad.Reader (ask)
import Control.Monad.ST (ST, runST)
import Data.List (sort)

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
        x      <- Memory.load mem $ Memory.register Memory.X
        cycles <- Memory.load mem Memory.cycles
        return $ (0x40, 106) @=? (x, cycles)

    , testCase "sum-squares.s" $ example "examples/sum-squares.s" $ \mem -> do
        x <- Memory.load mem $ Memory.register Memory.X
        return $ sum [n * n | n <- [0 .. 50]] @=? x

    , testCase "bubble-sort.s" $ example "examples/bubble-sort.s" $ \mem -> do
        xs <- forM [0 .. 9] $ Memory.load mem . Memory.ram . (0x1000 +)
        return $ sort xs @=? xs
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
