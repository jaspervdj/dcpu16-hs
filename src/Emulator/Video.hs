module Emulator.Video
    ( video
    ) where

import Data.Bits (shiftR, (.&.), (.|.))
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
        Ansi.setCursorPosition row col
        Ansi.setSGR $ color word
        putChar $ chr charcode
  where
    address'   = fromIntegral address
    videoStart = 0x8000
    videoEnd   = videoStart + terminalWidth * terminalHeight
    (row, col) = (address' - videoStart) `divMod` terminalWidth
    charcode   = fromIntegral $ word .&. 0x007f
video _ _ = return ()

color :: Word16 -> [Ansi.SGR]
color w
    | w .&. 0x0080 == 0x0000 = []
    | otherwise              =
        [ Ansi.SetColor Ansi.Foreground Ansi.Dull (toEnum fg)
        , Ansi.SetColor Ansi.Background Ansi.Dull (toEnum bg)
        ]
  where
    fg = fromIntegral $
        (w .&. 0x4000) `shiftR` 14 .|.
        (w .&. 0x2000) `shiftR` 12 .|.
        (w .&. 0x1000) `shiftR` 10
    bg = fromIntegral $
        (w .&. 0x0400) `shiftR` 10 .|.
        (w .&. 0x0200) `shiftR`  8 .|.
        (w .&. 0x0100) `shiftR`  6
