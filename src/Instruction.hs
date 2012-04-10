module Instruction
    ( BasicInstruction (..)
    , NonBasicInstruction (..)
    , Instruction (..)
    , decodeInstruction
    , encodeInstruction
    , instructionCycles
    , Operand (..)
    , decodeOperand
    , encodeOperand
    , operandCycles
    ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
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
    | UnknownInstruction Word16

instance Show a => Show (Instruction a) where
    show (BasicInstruction op a b)  = unwords [show op, show a, show b]
    show (NonBasicInstruction op a) = unwords [show op, show a]
    show (UnknownInstruction w)     = "??? (" ++ show w ++ ")"

decodeInstruction :: Word16 -> Instruction Operand
decodeInstruction word = case oooo of
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
        _    -> UnknownInstruction word

    -- Unknown instruction
    _   -> UnknownInstruction word
  where
    -- Word is of the form bbbbbbaaaaaaoooo
    oooo        = word .&. 0xf
    aaaaaa      = (word `shiftR` 4)  .&. 0x3f
    bbbbbb      = (word `shiftR` 10) .&. 0x3f
    a           = decodeOperand aaaaaa
    b           = decodeOperand bbbbbb

encodeInstruction :: Instruction Operand -> Word16
encodeInstruction (BasicInstruction op a b) =
    bbbbbb .|. aaaaaa .|. oooo
  where
    bbbbbb = encodeOperand b `shiftL` 10
    aaaaaa = encodeOperand a `shiftL` 4
    oooo   = case op of
        Set -> 0x1
        Add -> 0x2
        Sub -> 0x3
        Mul -> 0x4
        Div -> 0x5
        Mod -> 0x6
        Shl -> 0x7
        Shr -> 0x8
        And -> 0x9
        Bor -> 0xa
        Xor -> 0xb
        Ife -> 0xc
        Ifn -> 0xd
        Ifg -> 0xe
        Ifb -> 0xf
encodeInstruction (NonBasicInstruction op a) =
    aaaaaa .|. oooo
  where
    aaaaaa = encodeOperand a `shiftL` 10
    oooo   = (`shiftL` 4) $ case op of
        Jsr -> 0x01
encodeInstruction (UnknownInstruction word) = word

-- | How many cycles does the instruction take?
instructionCycles :: Instruction Operand -> Int
instructionCycles (BasicInstruction op a b) =
    operandCycles a + operandCycles b + case op of
        Set -> 1
        Add -> 2
        Sub -> 2
        Mul -> 2
        Div -> 3
        Mod -> 3
        Shl -> 2
        Shr -> 2
        And -> 1
        Bor -> 1
        Xor -> 1
        Ife -> 2
        Ifn -> 2
        Ifg -> 2
        Ifb -> 2
instructionCycles (NonBasicInstruction op a) =
    operandCycles a + case op of
        Jsr -> 2
instructionCycles (UnknownInstruction _) = 0

data Operand
    = ORegister Register
    | OPRegister Register
    | OPNextWordPlusRegister Register
    | OLiteral Word16
    | OPop
    | OPeek
    | OPush
    | OSp
    | OPc
    | OO
    | OPNextWord
    | ONextWord

instance Show Operand where
    show (ORegister r)              = show r
    show (OPRegister r)             = "[" ++ show r ++ "]"
    show (OPNextWordPlusRegister r) = "[next word + " ++ show r ++ "]"
    show (OLiteral l)               = prettifyWord16 l
    show OPop                       = "Pop"
    show OPeek                      = "Peek"
    show OPush                      = "Push"
    show OSp                        = "Sp"
    show OPc                        = "Pc"
    show OO                         = "O"
    show OPNextWord                 = "[next word]"
    show ONextWord                  = "(next word)"

-- | Only looks at the 6 least significant bits
decodeOperand :: Word16 -> Operand
decodeOperand word
    | word <= 0x07 = ORegister $ reg word
    | word <= 0x0f = OPRegister $ reg $ word - 0x08
    | word <= 0x17 = OPNextWordPlusRegister $ reg $ word - 0x10
    | word >= 0x20 = OLiteral $ word - 0x20
    | otherwise    = case word of
        0x18 -> OPop
        0x19 -> OPeek
        0x1a -> OPush
        0x1b -> OSp
        0x1c -> OPc
        0x1d -> OO
        0x1e -> OPNextWord
        0x1f -> ONextWord
        _    -> error $ "Unknown operand: " ++ prettifyWord16 word
  where
    reg = toEnum . fromIntegral

encodeOperand :: Operand -> Word16
encodeOperand operand = case operand of
    ORegister reg              -> unreg reg
    OPRegister reg             -> 0x08 + unreg reg
    OPNextWordPlusRegister reg -> 0x10 + unreg reg
    OLiteral w                 -> 0x20 + w  -- TODO: check if w < 0x31?
    OPop                       -> 0x18
    OPeek                      -> 0x19
    OPush                      -> 0x1a
    OSp                        -> 0x1b
    OPc                        -> 0x1c
    OO                         -> 0x1d
    OPNextWord                 -> 0x1e
    ONextWord                  -> 0x1f
  where
    unreg = fromIntegral . fromEnum

operandCycles :: Operand -> Int
operandCycles operand = case operand of
    OPNextWordPlusRegister _ -> 1
    OPNextWord               -> 1
    ONextWord                -> 1
    _                        -> 0
