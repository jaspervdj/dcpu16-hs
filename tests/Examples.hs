{-# LANGUAGE Rank2Types #-}
module Examples
    ( tests
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM)
import Data.Bits (shiftL)
import Data.List (isPrefixOf, sort)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assert, (@=?))
import qualified Data.ByteString as B

import Assembler
import Emulator
import Emulator.Monad
import Emulator.Monad.ST
import Memory (Address (..), Register (..))

tests :: Test
tests = testGroup "Examples"
    [ testExample "notch" $ do
        x      <- load $ Register X
        cycles <- load Cycles
        return $ (0x40, 106) @=? (x, cycles)

    , testExample "sum-squares" $ do
        x <- load $ Register X
        return $ sum [n * n | n <- [0 .. 50]] @=? x

    , testExample "bubble-sort" $ do
        xs <- forM [0 .. 9] $ load . Ram . (0x1000 +)
        return $ sort xs @=? xs

    , testExample "32-bit-add" $ do
        lo <- load $ Ram 0x1000
        hi <- load $ Ram 0x1001
        let sum' = (fromIntegral hi `shiftL` 16) + fromIntegral lo :: Int
        return $ 0x12345678 + 0xaabbccdd @=? sum'

    , testExample "fib" $ do
        let fibs  = 1 : 2 : zipWith (+) fibs (tail fibs)
            addrs = [0xffff, 0xfffe .. 0x000c]

            loop _        []       = return True
            loop []       _        = return True
            loop (f : fs) (a : as) = do
                f' <- load $ Ram a
                if f == f' then loop fs as else return False

        return . assert =<< loop fibs addrs

    , testExample "self-copy" $ do
        let readRam i = do
                x <- load $ Ram i
                if x == 0x0000
                    then return []
                    else (x :) <$> readRam (i + 1)

        programs <- readRam 1
        let len      = length programs `div` 10
            equal xs = case splitAt len xs of
                (_, [])  -> True
                (hs, ts) -> hs `isPrefixOf` ts && equal ts

        return $ assert $ equal programs
    ]

testExample :: String
            -> (forall s. STEmulator s Assertion)
            -> Test
testExample name = testCase name .
    example ("examples/" ++ name ++ ".dasm16")


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
