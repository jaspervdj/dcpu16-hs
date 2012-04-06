module Instruction
    ( BasicInstruction (..)
    , NonBasicInstruction (..)
    , Instruction (..)
    , parseInstruction
    , Operand (..)
    , parseOperand
    ) where

import Data.Bits (shiftR, (.&.))
import Data.Word (Word16)

import Memory
import Util

data BasicInstruction
    = Set
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Shl
    | Shr
    | And
    | Bor
    | Xor
    | Ife
    | Ifn
    | Ifg
    | Ifb
    deriving (Show)

data NonBasicInstruction
    = Jsr
    deriving (Show)

data Instruction a
    = BasicInstruction BasicInstruction a a
    | NonBasicInstruction NonBasicInstruction a
    deriving (Show)

parseInstruction :: Word16 -> Instruction Operand
parseInstruction word = case oooo of
    -- Basic instructions
    0x1 -> BasicInstruction Set a b
    0x2 -> BasicInstruction Add a b
    0x3 -> BasicInstruction Sub a b
    0x4 -> BasicInstruction Mul a b
    0x5 -> BasicInstruction Div a b
    0x6 -> BasicInstruction Mod a b
    0x7 -> BasicInstruction Shl a b
    0x8 -> BasicInstruction Shr a b
    0x9 -> BasicInstruction And a b
    0xa -> BasicInstruction Bor a b
    0xb -> BasicInstruction Xor a b
    0xc -> BasicInstruction Ife a b
    0xd -> BasicInstruction Ifn a b
    0xe -> BasicInstruction Ifg a b
    0xf -> BasicInstruction Ifb a b

    -- Non-basic instructions
    0x0 -> case aaaaaa of
        0x01 -> NonBasicInstruction Jsr b
        _    -> error $ "Unknown non-basic opcode: " ++ prettifyWord16 aaaaaa

    -- Unknown instruction
    _   -> error $ "unknown basic opcode: " ++ prettifyWord16 oooo
  where
    -- Word is of the form bbbbbbaaaaaaoooo
    oooo        = word .&. 0xf
    aaaaaa      = (word `shiftR` 4)  .&. 0x3f
    bbbbbb      = (word `shiftR` 10) .&. 0x3f
    a           = parseOperand aaaaaa
    b           = parseOperand bbbbbb

data Operand
    = ORegister Register
    | ORamAtRegister Register
    | ORamAtNextWordPlusRegister Register
    | OPop
    | OPeek
    | OPush
    | OSp
    | OPc
    | OO
    | ORamAtNextWord
    | ONextWord
    | OLiteral Word16
    deriving (Show)

-- | Only looks at the 6 least significant bits
parseOperand :: Word16 -> Operand
parseOperand word
    | word <= 0x07 = ORegister $ reg word
    | word <= 0x0f = ORamAtRegister $ reg $ word - 0x08
    | word <= 0x17 = ORamAtNextWordPlusRegister $ reg $ word - 0x10
    | word >= 0x20 = OLiteral $ word - 0x20
    | otherwise    = case word of
        0x18 -> OPop
        0x19 -> OPeek
        0x1a -> OPush
        0x1b -> OSp
        0x1c -> OPc
        0x1d -> OO
        0x1e -> ORamAtNextWord
        0x1f -> ONextWord
        _    -> error $ "Unknown operand: " ++ prettifyWord16 word
  where
    reg = toEnum . fromIntegral
