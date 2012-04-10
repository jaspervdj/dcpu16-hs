{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
module Memory
    ( -- * Addresses
      Register (..)
    , Address (..)
    

      -- * Talking to the memory
    , Memory
    , new
    , load
    , store
    ) where

import GHC.Base (Int (..))
import GHC.Prim
import GHC.ST (ST (..))
import GHC.Word (Word16 (..))

data Register = A | B | C | X | Y | Z | I | J
    deriving (Bounded, Enum, Show)

data Address
    = Pc
    | Sp
    | O
    | Skip
    | Cycles
    | Register Register
    | Ram Word16
    deriving (Show)

fromAddress :: Address -> Int
fromAddress Pc           = 0x0
fromAddress Sp           = 0x1
fromAddress O            = 0x2
fromAddress Skip         = 0x3
fromAddress Cycles       = 0x4
fromAddress (Register r) = 0x8  + fromEnum r
fromAddress (Ram r)      = 0x16 + fromIntegral r

data Memory s = Memory (MutableByteArray# s)

new :: ST s (Memory s)
new = do
    mem <- new'
    store mem Pc     0x0000
    store mem Sp     0xffff
    store mem O      0x0000
    store mem Skip   0x0000
    store mem Cycles 0x0000
    return mem

new' :: ST s (Memory s)
new' = ST $ \s1# ->
    case newByteArray# (len# *# 2#) s1# of
        (# s2#, marr# #) -> (# s2#, Memory marr# #)
  where
    !(I# len#) = 0x8 + 0x8 + 0x10000

load :: Memory s -> Address -> ST s Word16
load (Memory marr#) address = ST $ \s1# ->
    case readWord16Array# marr# i# s1# of
        (# s2#, w16# #) -> (# s2#, W16# w16# #)
  where
    !(I# i#) = fromAddress address

store :: Memory s -> Address -> Word16 -> ST s ()
store (Memory marr#) address (W16# w16#) = ST $ \s1# ->
    case writeWord16Array# marr# i# w16# s1# of
         s2# -> (# s2#, () #)
  where
    !(I# i#) = fromAddress address
