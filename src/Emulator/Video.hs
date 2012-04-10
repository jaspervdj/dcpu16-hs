module Emulator.Video
    ( video
    ) where

import Data.Bits ((.&.))
import Data.Char (chr)
import Data.Word (Word16)

import Memory (Address (..))
import qualified System.Console.ANSI as Ansi

terminalWidth :: Int
terminalWidth = 78

terminalHeight :: Int
terminalHeight = 32

video :: Address -> Word16 -> IO ()
video (Ram address) word
    | address' < videoStart = return ()
    | address' >= videoEnd  = return ()
    | otherwise             = do
        Ansi.setCursorPosition row column
        putChar $ chr charcode
  where
    address'   = fromIntegral address
    videoStart = 0x8000
    videoEnd   = videoStart + terminalWidth * terminalHeight
    row        = (address' - videoStart) `div` terminalWidth
    column     = (address' - videoStart) `mod` terminalWidth
    charcode   = fromIntegral $ word .&. 0x007f
video _ _ = return ()
