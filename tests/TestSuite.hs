{-# LANGUAGE Rank2Types #-}
module Main where

import Test.Framework (defaultMain)

import qualified Examples as Examples

main :: IO ()
main = defaultMain
    [ Examples.tests
    ]
