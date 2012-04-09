{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
module Memory
    ( -- * Core memory type
      Memory
    , Address
    , new
    , load
    , store
      
      -- * Address translation
    , pc
    , sp
    , o
    , skip
    , cycles

    , Register (..)
    , register
    , ram
    ) where

import GHC.Base (Int (..))
import GHC.Prim
import GHC.ST (ST (..))
import GHC.Word (Word16 (..))

data Memory s = Memory (MutableByteArray# s)

type Address = Int

new :: ST s (Memory s)
new = do
    mem <- new'
    store mem pc     0x0000
    store mem sp     0xffff
    store mem o      0x0000
    store mem skip   0x0000
    store mem cycles 0x0000
    return mem

new' :: ST s (Memory s)
new' = ST $ \s1# ->
    case newByteArray# (len# *# 2#) s1# of
        (# s2#, marr# #) -> (# s2#, Memory marr# #)
  where
    !(I# len#) = 0x8 + 0x8 + 0x10000

load :: Memory s -> Address -> ST s Word16
load (Memory marr#) (I# i#) = ST $ \s1# ->
    case readWord16Array# marr# i# s1# of
        (# s2#, w16# #) -> (# s2#, W16# w16# #)

store :: Memory s -> Address -> Word16 -> ST s ()
store (Memory marr#) (I# i#) (W16# w16#) = ST $ \s1# ->
    case writeWord16Array# marr# i# w16# s1# of
         s2# -> (# s2#, () #)

pc :: Address
pc = 0x0

sp :: Address
sp = 0x1

o :: Address
o = 0x2

skip :: Address
skip = 0x3

cycles :: Address
cycles = 0x4

data Register = A | B | C | X | Y | Z | I | J
    deriving (Bounded, Enum, Show)

register :: Register -> Address
register reg = fromEnum reg + 0x8

ram :: Word16 -> Address
ram (W16# ra#) = I# (word2Int# ra# +# 0x8# +# 0x8#)
