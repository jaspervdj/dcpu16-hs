module Util
    ( prettifyWord16
    ) where

import Data.Word (Word16)
import Text.Printf (printf)

prettifyWord16 :: Word16 -> String
prettifyWord16 = printf "%04x"
