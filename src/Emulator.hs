{-# LANGUAGE BangPatterns, Rank2Types #-}
module Emulator
    ( loadProgram
    , emulate
    , loadInstruction
    , loadOperands
    , execute
    , prettify
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.Word (Word, Word16)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Emulator.Monad
import Instruction
import Memory (Address)
import Util
import qualified Memory as Memory

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
            store (Memory.ram addr) w16
            loop (i + 2)

loadNextWord :: MonadEmulator m => m Word16
loadNextWord = do
    pc  <- load Memory.pc
    pcv <- load (Memory.ram pc)
    store Memory.pc (pc + 1)
    return pcv

addCycles :: MonadEmulator m => Int -> m ()
addCycles c = do
    cycles <- load Memory.cycles
    store Memory.cycles (cycles + fromIntegral c)

-- | After we load an operand, we get a value. This is either an address (we
-- can write back to) or a literal value.
data Value
    = Address Address
    | Literal Word16
    deriving (Show)

loadOperand :: MonadEmulator m => Operand -> m Value
loadOperand (ORegister reg) =
    return $ Address $ Memory.register reg
loadOperand (OPRegister reg) = do
    regv <- load (Memory.register reg)
    return $ Address $ Memory.ram regv
loadOperand (OPNextWordPlusRegister reg) = do
    nw   <- loadNextWord
    regv <- load (Memory.register reg)
    return $ Address $ Memory.ram $ nw + regv
loadOperand OPop = do
    sp  <- load Memory.sp
    store Memory.sp (sp + 1)
    return $ Address $ Memory.ram sp
loadOperand OPeek = do
    sp  <- load Memory.sp
    return $ Address $ Memory.ram sp
loadOperand OPush = do
    sp' <- fmap (flip (-) 1) $ load Memory.sp
    store Memory.sp sp'
    return $ Address $ Memory.ram sp'
loadOperand OSp =
    return $ Address $ Memory.sp
loadOperand OPc =
    return $ Address $ Memory.pc
loadOperand OO = do
    return $ Address $ Memory.o
loadOperand OPNextWord = do
    nw <- loadNextWord
    return $ Address $ Memory.ram nw
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

-- | Stops when an unknown instruction is encountered
emulate :: MonadEmulator m => m ()
emulate = do
    instr <- loadInstruction
    case instr of
        UnknownInstruction _ -> return ()
        _                    -> do
            loadOperands instr >>= execute instr
            emulate

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
    skip <- load Memory.skip
    if (skip == 0x0000)
        then do
            execute' instr'
            addCycles $ instructionCycles instr
        else do
            store Memory.skip 0x0000
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
    store Memory.o (if overflow then 0x0001 else 0x0000)
execute' (BasicInstruction Sub a b) = do
    x <- loadValue a
    y <- loadValue b
    let (x', y')  = (fromIntegral x, fromIntegral y)
        underflow = x' - y' < (0x0000 :: Int)
    storeValue a (x - y)
    store Memory.o (if underflow then 0xffff else 0x0000)
execute' (BasicInstruction Mul a b) = do
    x <- loadValue a
    y <- loadValue b
    let (x', y') = (fromIntegral x, fromIntegral y)
        overflow = ((x' * y') `shiftR` 16) .&. 0xffff :: Word
    storeValue a (x * y)
    store Memory.o (fromIntegral overflow)
execute' (BasicInstruction Div a b) = do
    x <- loadValue a
    y <- loadValue b
    if y == 0x0000
        then do
            storeValue a 0x0000
            store Memory.o 0x0000
        else do
            let (x', y') = (fromIntegral x, fromIntegral y)
                overflow = ((x' `shiftL` 16) `div` y') .&. 0xffff :: Word
            storeValue a (x `div` y)
            store Memory.o (fromIntegral overflow)
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
    store Memory.o (fromIntegral overflow)
execute' (BasicInstruction Shr a b) = do
    x <- loadValue a
    y <- loadValue b
    let (x', y') = (fromIntegral x, fromIntegral y)
        overflow = ((x' `shiftL` 16) `shiftR` y') .&. 0xffff :: Word
    storeValue a (x `shiftR` y')
    store Memory.o (fromIntegral overflow)
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
    store Memory.skip (if x == y then 0x0000 else 0x0001)
execute' (BasicInstruction Ifn a b) = do
    x <- loadValue a
    y <- loadValue b
    store Memory.skip (if x /= y then 0x0000 else 0x0001)
execute' (BasicInstruction Ifg a b) = do
    x <- loadValue a
    y <- loadValue b
    store Memory.skip (if x > y then 0x0000 else 0x0001)
execute' (BasicInstruction Ifb a b) = do
    x <- loadValue a
    y <- loadValue b
    store Memory.skip $ if (x .&. y) == 0 then 0x0000 else 0x0001
execute' (NonBasicInstruction Jsr a) = do
    pcv  <- load Memory.pc
    x    <- loadValue a
    addr <- loadOperand OPush
    execute' $ BasicInstruction Set addr (Literal pcv)  -- Push address on stack
    store Memory.pc x                                   -- Set PC to a (jump)
execute' (UnknownInstruction _) =
    return ()

prettify :: MonadEmulator m => m String
prettify = unlines . concat <$>
    sequence [prettifyEmulator, prettifyRegister, prettifyRam]

prettifyEmulator :: MonadEmulator m => m [String]
prettifyEmulator = do
    pc     <- load Memory.pc
    sp     <- load Memory.sp
    o      <- load Memory.o
    skip   <- load Memory.skip
    cycles <- load Memory.cycles
    return $
        [ "EMULATOR"
        , ""
        , "PC:     " ++ prettifyWord16 pc
        , "SP:     " ++ prettifyWord16 sp
        , "O:      " ++ prettifyWord16 o
        , "SKIP:   " ++ prettifyWord16 skip
        , "CYCLES: " ++ prettifyWord16 cycles
        , ""
        ]

prettifyRegister :: MonadEmulator m => m [String]
prettifyRegister = do
    registers <- forM [minBound .. maxBound] $ \name -> do
        val <- load (Memory.register name)
        return (name, val)
    return $
        ["REGISTER", ""] ++
        [show name ++ ": " ++ prettifyWord16 val | (name, val) <- registers] ++
        [""]

prettifyRam :: MonadEmulator m => m [String]
prettifyRam = do
    ls <- mapM line [(x * 8, x * 8 + 7) | x <- [0 .. 0xffff `div` 8]]
    return $ ["RAM", ""] ++ ls ++ [""]
  where
    line (lo, up) = do
        vals <- mapM (load . Memory.ram) [lo .. up]
        return $ prettifyWord16 lo ++ ": " ++ unwords (map prettifyWord16 vals)
