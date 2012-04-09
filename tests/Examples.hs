{-# LANGUAGE Rank2Types #-}
module Examples
    ( tests
    ) where

import Control.Monad (forM)
import Data.Bits (shiftL)
import Data.List (sort)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))
import qualified Data.ByteString as B

import Assembler
import Emulator
import Emulator.Monad
import Emulator.Monad.ST
import qualified Memory as Memory

tests :: Test
tests = testGroup "Examples"
    [ testCase "notch.s" $ example "examples/notch.s" $ do
        x      <- load $ Memory.register Memory.X
        cycles <- load Memory.cycles
        return $ (0x40, 106) @=? (x, cycles)

    , testCase "sum-squares.s" $ example "examples/sum-squares.s" $ do
        x <- load $ Memory.register Memory.X
        return $ sum [n * n | n <- [0 .. 50]] @=? x

    , testCase "bubble-sort.s" $ example "examples/bubble-sort.s" $ do
        xs <- forM [0 .. 9] $ load . Memory.ram . (0x1000 +)
        return $ sort xs @=? xs

    , testCase "32-bit-add.s" $ example "examples/32-bit-add.s" $ do
        lo <- load $ Memory.ram 0x1000
        hi <- load $ Memory.ram 0x1001
        let sum' = (fromIntegral hi `shiftL` 16) + fromIntegral lo :: Int
        return $ 0x12345678 + 0xaabbccdd @=? sum'
    ]

example :: FilePath
        -> (forall s. STEmulator s Assertion)
        -> Assertion
example filePath check = do
    assembleFile filePath "a.out"
    program <- B.readFile "a.out"
    runSTEmulator $ do
        loadProgram program
        emulate
        check
