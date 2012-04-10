{-# LANGUAGE BangPatterns, Rank2Types #-}
module Emulator
    ( Value (..)
    , loadProgram
    , emulate
    , emulateWith
    , loadInstruction
    , loadOperands
    , execute
    ) where

import Control.Applicative ((<$>))
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.Word (Word, Word16)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Emulator.Monad
import Instruction
import Memory (Address (..))
import Util

-- | After we load an operand, we get a value. This is either an address (we
-- can write back to) or a literal value.
data Value
    = Address Address
    | Literal Word16

instance Show Value where
    show (Address a) = "[" ++ show a ++ "]"
    show (Literal l) = prettifyWord16 l

-- | Load a program from a bytestring
loadProgram :: MonadEmulator m => ByteString -> m ()
loadProgram bs = loop 0
  where
    len = B.length bs
    loop !i
        | i + 1 >= len = return ()
        | otherwise    = do
            let !b1   = fromIntegral $ B.index bs i
                !b2   = fromIntegral $ B.index bs (i + 1)
                !w16  = (b1 `shiftL` 8) + b2
                !addr = fromIntegral $ i `div` 2
            store (Ram addr) w16
            loop (i + 2)

loadNextWord :: MonadEmulator m => m Word16
loadNextWord = do
    pc  <- load Pc
    pcv <- load (Ram pc)
    store Pc (pc + 1)
    return pcv

addCycles :: MonadEmulator m => Int -> m ()
addCycles c = do
    cycles <- load Cycles
    store Cycles (cycles + fromIntegral c)

loadOperand :: MonadEmulator m => Operand -> m Value
loadOperand (ORegister reg) =
    return $ Address $ Register reg
loadOperand (OPRegister reg) = do
    regv <- load (Register reg)
    return $ Address $ Ram regv
loadOperand (OPNextWordPlusRegister reg) = do
    nw   <- loadNextWord
    regv <- load (Register reg)
    return $ Address $ Ram $ nw + regv
loadOperand OPop = do
    sp  <- load Sp
    store Sp (sp + 1)
    return $ Address $ Ram sp
loadOperand OPeek = do
    sp  <- load Sp
    return $ Address $ Ram sp
loadOperand OPush = do
    sp' <- fmap (flip (-) 1) $ load Sp
    store Sp sp'
    return $ Address $ Ram sp'
loadOperand OSp =
    return $ Address $ Sp
loadOperand OPc =
    return $ Address $ Pc
loadOperand OO = do
    return $ Address $ O
loadOperand OPNextWord = do
    nw <- loadNextWord
    return $ Address $ Ram nw
loadOperand ONextWord = do
    nw <- loadNextWord
    return $ Literal nw
loadOperand (OLiteral w) =
    return $ Literal w

loadValue :: MonadEmulator m => Value -> m Word16
loadValue (Address address) = load address
loadValue (Literal w)       = return w

storeValue :: MonadEmulator m => Value -> Word16 -> m ()
storeValue (Address address) val = store address val
storeValue (Literal _)       _   = return ()

emulate :: MonadEmulator m => m ()
emulate = emulateWith $ const $ const $ return ()

-- | Stops when an unknown instruction is encountered
emulateWith :: MonadEmulator m
            => (Instruction Operand -> Instruction Value -> m ())
            -> m ()
emulateWith callback = do
    instr <- loadInstruction
    case instr of
        UnknownInstruction _ -> return ()
        _                    -> do
            instr' <- loadOperands instr
            execute instr instr'
            callback instr instr'
            emulateWith callback

loadInstruction :: MonadEmulator m => m (Instruction Operand)
loadInstruction = decodeInstruction <$> loadNextWord

loadOperands :: MonadEmulator m => Instruction Operand -> m (Instruction Value)
loadOperands (BasicInstruction op a b) = do
    av <- loadOperand a
    bv <- loadOperand b
    return $ BasicInstruction op av bv
loadOperands (NonBasicInstruction op a) = do
    av <- loadOperand a
    return $ NonBasicInstruction op av
loadOperands (UnknownInstruction w) =
    return $ UnknownInstruction w

execute :: MonadEmulator m => Instruction Operand -> Instruction Value -> m ()
execute instr instr' = do
    skip <- load Skip
    if (skip == 0x0000)
        then do
            execute' instr'
            addCycles $ instructionCycles instr
        else do
            store Skip 0x0000
            addCycles 1

execute' :: MonadEmulator m => Instruction Value -> m ()
execute' (BasicInstruction Set a b) = do
    x <- loadValue b
    storeValue a x
execute' (BasicInstruction Add a b) = do
    x <- loadValue a
    y <- loadValue b
    let (x', y') = (fromIntegral x, fromIntegral y)
        overflow = x' + y' > (0xffff :: Int)
    storeValue a (x + y)
    store O (if overflow then 0x0001 else 0x0000)
execute' (BasicInstruction Sub a b) = do
    x <- loadValue a
    y <- loadValue b
    let (x', y')  = (fromIntegral x, fromIntegral y)
        underflow = x' - y' < (0x0000 :: Int)
    storeValue a (x - y)
    store O (if underflow then 0xffff else 0x0000)
execute' (BasicInstruction Mul a b) = do
    x <- loadValue a
    y <- loadValue b
    let (x', y') = (fromIntegral x, fromIntegral y)
        overflow = ((x' * y') `shiftR` 16) .&. 0xffff :: Word
    storeValue a (x * y)
    store O (fromIntegral overflow)
execute' (BasicInstruction Div a b) = do
    x <- loadValue a
    y <- loadValue b
    if y == 0x0000
        then do
            storeValue a 0x0000
            store O 0x0000
        else do
            let (x', y') = (fromIntegral x, fromIntegral y)
                overflow = ((x' `shiftL` 16) `div` y') .&. 0xffff :: Word
            storeValue a (x `div` y)
            store O (fromIntegral overflow)
execute' (BasicInstruction Mod a b) = do
    x <- loadValue a
    y <- loadValue b
    if y == 0x0000
        then storeValue a 0x0000
        else storeValue a (x `mod` y)
execute' (BasicInstruction Shl a b) = do
    x <- loadValue a
    y <- loadValue b
    let (x', y') = (fromIntegral x, fromIntegral y)
        overflow = ((x' `shiftL` y') `shiftR` 16) .&. 0xffff :: Word
    storeValue a (x `shiftL` y')
    store O (fromIntegral overflow)
execute' (BasicInstruction Shr a b) = do
    x <- loadValue a
    y <- loadValue b
    let (x', y') = (fromIntegral x, fromIntegral y)
        overflow = ((x' `shiftL` 16) `shiftR` y') .&. 0xffff :: Word
    storeValue a (x `shiftR` y')
    store O (fromIntegral overflow)
execute' (BasicInstruction And a b) = do
    x <- loadValue a
    y <- loadValue b
    storeValue a (x .&. y)
execute' (BasicInstruction Bor a b) = do
    x <- loadValue a
    y <- loadValue b
    storeValue a (x .|. y)
execute' (BasicInstruction Xor a b) = do
    x <- loadValue a
    y <- loadValue b
    storeValue a (xor x y)
execute' (BasicInstruction Ife a b) = do
    x <- loadValue a
    y <- loadValue b
    store Skip (if x == y then 0x0000 else 0x0001)
execute' (BasicInstruction Ifn a b) = do
    x <- loadValue a
    y <- loadValue b
    store Skip (if x /= y then 0x0000 else 0x0001)
execute' (BasicInstruction Ifg a b) = do
    x <- loadValue a
    y <- loadValue b
    store Skip (if x > y then 0x0000 else 0x0001)
execute' (BasicInstruction Ifb a b) = do
    x <- loadValue a
    y <- loadValue b
    store Skip $ if (x .&. y) == 0 then 0x0000 else 0x0001
execute' (NonBasicInstruction Jsr a) = do
    pcv  <- load Pc
    x    <- loadValue a
    addr <- loadOperand OPush
    execute' $ BasicInstruction Set addr (Literal pcv)  -- Push address on stack
    store Pc x                                          -- Set PC to a (jump)
execute' (UnknownInstruction _) =
    return ()
