{-# LANGUAGE BangPatterns, Rank2Types #-}
module Emulator
    ( EmulatorM
    , newEmulatorState
    , runEmulatorM
    , loadProgram
    , step
    , loadInstruction
    , loadOperands
    , execute
    , prettify
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.ST (ST)
import Control.Monad.Trans (lift)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.Word (Word, Word16)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Instruction
import Memory (Address, Memory)
import Util
import qualified Memory as Memory

type EmulatorState s = Memory s

newEmulatorState :: ST s (EmulatorState s)
newEmulatorState = do
    mem <- Memory.new
    Memory.store mem Memory.sp 0xffff
    return mem

type EmulatorM s = ReaderT (EmulatorState s) (ST s)

runEmulatorM :: EmulatorM s a -> EmulatorState s -> ST s a
runEmulatorM = runReaderT

-- | Load a program from a bytestring
loadProgram :: ByteString -> EmulatorM s ()
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
            mem <- ask
            lift $ Memory.store mem (Memory.ram addr) w16
            loop (i + 2)

loadNextWord :: EmulatorM s Word16
loadNextWord = do
    mem <- ask
    pc  <- lift $ Memory.load mem Memory.pc
    pcv <- lift $ Memory.load mem (Memory.ram pc)
    lift $ Memory.store mem Memory.pc (pc + 1)
    return pcv

-- | After we load an operand, we get a value. This is either an address (we
-- can write back to) or a literal value.
data Value
    = Address Address
    | Literal Word16
    deriving (Show)

loadOperand :: Operand -> EmulatorM s Value
loadOperand (ORegister reg) =
    return $ Address $ Memory.register reg
loadOperand (OPRegister reg) = do
    mem  <- ask
    regv <- lift $ Memory.load mem (Memory.register reg)
    return $ Address $ Memory.ram regv
loadOperand (OPNextWordPlusRegister reg) = do
    mem  <- ask
    nw   <- loadNextWord
    regv <- lift $ Memory.load mem (Memory.register reg)
    return $ Address $ Memory.ram $ nw + regv
loadOperand OPop = do
    mem <- ask
    sp  <- lift $ Memory.load mem Memory.sp
    lift $ Memory.store mem Memory.sp (sp + 1)
    return $ Address $ Memory.ram sp
loadOperand OPeek = do
    mem <- ask
    sp  <- lift $ Memory.load mem Memory.sp
    return $ Address $ Memory.ram sp
loadOperand OPush = do
    mem <- ask
    sp' <- fmap (flip (-) 1) $ lift $ Memory.load mem Memory.sp
    lift $ Memory.store mem Memory.sp sp'
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

loadValue :: Value -> EmulatorM s Word16
loadValue (Address address) = do
    mem <- ask
    lift $ Memory.load mem address
loadValue (Literal w) = return w

storeValue :: Value -> Word16 -> EmulatorM s ()
storeValue (Address address) val = do
    mem <- ask
    lift $ Memory.store mem address val
storeValue (Literal _) _ = return ()

step :: EmulatorM s ()
step = loadInstruction >>= loadOperands >>= execute

loadInstruction :: EmulatorM s (Instruction Operand)
loadInstruction = decodeInstruction <$> loadNextWord

loadOperands :: Instruction Operand -> EmulatorM s (Instruction Value)
loadOperands (BasicInstruction op a b) = do
    av <- loadOperand a
    bv <- loadOperand b
    return $ BasicInstruction op av bv
loadOperands (NonBasicInstruction op a) = do
    av <- loadOperand a
    return $ NonBasicInstruction op av

execute :: Instruction Value -> EmulatorM s ()
execute instruction = do
    mem  <- ask
    skip <- lift $ Memory.load mem Memory.skip

    if (skip == 0x0000)
        then execute' instruction
        else lift $ Memory.store mem Memory.skip 0x0000

execute' :: Instruction Value -> EmulatorM s ()
execute' (BasicInstruction Set a b) = do
    x <- loadValue b
    storeValue a x
execute' (BasicInstruction Add a b) = do
    mem <- ask
    x   <- loadValue a
    y   <- loadValue b
    let (x', y') = (fromIntegral x, fromIntegral y)
        overflow = x' + y' > (0xffff :: Int)
    storeValue a (x + y)
    lift $ Memory.store mem Memory.o (if overflow then 0x0001 else 0x0000)
execute' (BasicInstruction Sub a b) = do
    mem <- ask
    x   <- loadValue a
    y   <- loadValue b
    let (x', y')  = (fromIntegral x, fromIntegral y)
        underflow = x' - y' < (0x0000 :: Int)
    storeValue a (x - y)
    lift $ Memory.store mem Memory.o (if underflow then 0xffff else 0x0000)
execute' (BasicInstruction Mul a b) = do
    mem <- ask
    x   <- loadValue a
    y   <- loadValue b
    let (x', y') = (fromIntegral x, fromIntegral y)
        overflow = ((x' * y') `shiftR` 16) .&. 0xffff :: Word
    storeValue a (x * y)
    lift $ Memory.store mem Memory.o (fromIntegral overflow)
execute' (BasicInstruction Div a b) = do
    mem <- ask
    x   <- loadValue a
    y   <- loadValue b
    if y == 0x0000
        then do
            storeValue a 0x0000
            lift $ Memory.store mem Memory.o 0x0000
        else do
            let (x', y') = (fromIntegral x, fromIntegral y)
                overflow = ((x' `shiftL` 16) `div` y') .&. 0xffff :: Word
            storeValue a (x `div` y)
            lift $ Memory.store mem Memory.o (fromIntegral overflow)
execute' (BasicInstruction Mod a b) = do
    x <- loadValue a
    y <- loadValue b
    if y == 0x0000
        then storeValue a 0x0000
        else storeValue a (x `mod` y)
execute' (BasicInstruction Shl a b) = do
    mem <- ask
    x   <- loadValue a
    y   <- loadValue b
    let (x', y') = (fromIntegral x, fromIntegral y)
        overflow = ((x' `shiftL` y') `shiftR` 16) .&. 0xffff :: Word
    storeValue a (x `shiftL` y')
    lift $ Memory.store mem Memory.o (fromIntegral overflow)
execute' (BasicInstruction Shr a b) = do
    mem <- ask
    x   <- loadValue a
    y   <- loadValue b
    let (x', y') = (fromIntegral x, fromIntegral y)
        overflow = ((x' `shiftL` 16) `shiftR` y') .&. 0xffff :: Word
    storeValue a (x `shiftR` y')
    lift $ Memory.store mem Memory.o (fromIntegral overflow)
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
    mem <- ask
    x   <- loadValue a
    y   <- loadValue b
    lift $ Memory.store mem Memory.skip (if x == y then 0x0000 else 0x0001)
execute' (BasicInstruction Ifn a b) = do
    mem <- ask
    x   <- loadValue a
    y   <- loadValue b
    lift $ Memory.store mem Memory.skip (if x /= y then 0x0000 else 0x0001)
execute' (BasicInstruction Ifg a b) = do
    mem <- ask
    x   <- loadValue a
    y   <- loadValue b
    lift $ Memory.store mem Memory.skip (if x > y then 0x0000 else 0x0001)
execute' (BasicInstruction Ifb a b) = do
    mem <- ask
    x   <- loadValue a
    y   <- loadValue b
    lift $ Memory.store mem Memory.skip $
        if (x .&. y) == 0 then 0x0000 else 0x0001
execute' (NonBasicInstruction Jsr a) = do
    mem  <- ask
    pcv  <- lift $ Memory.load mem Memory.pc
    x    <- loadValue a
    addr <- loadOperand OPush
    execute' $ BasicInstruction Set addr (Literal pcv)  -- Push address on stack
    lift $ Memory.store mem Memory.pc x                 -- Set PC to a (jump)

prettify :: EmulatorM s String
prettify = unlines . concat <$>
    sequence [prettifyEmulator, prettifyRegister, prettifyRam]

prettifyEmulator :: EmulatorM s [String]
prettifyEmulator = do
    mem  <- ask
    pc   <- lift $ Memory.load mem Memory.pc
    sp   <- lift $ Memory.load mem Memory.sp
    o    <- lift $ Memory.load mem Memory.o
    skip <- lift $ Memory.load mem Memory.skip
    return $
        [ "EMULATOR"
        , ""
        , "PC:   " ++ prettifyWord16 pc
        , "SP:   " ++ prettifyWord16 sp
        , "O:    " ++ prettifyWord16 o
        , "SKIP: " ++ prettifyWord16 skip
        , ""
        ]

prettifyRegister :: EmulatorM s [String]
prettifyRegister = do
    mem <- ask
    registers   <- forM [minBound .. maxBound] $ \name -> do
        val <- lift $ Memory.load mem (Memory.register name)
        return (name, val)
    return $
        ["REGISTER", ""] ++
        [show name ++ ": " ++ prettifyWord16 val | (name, val) <- registers] ++
        [""]

prettifyRam :: EmulatorM s [String]
prettifyRam = do
    ls <- mapM line [(x * 8, x * 8 + 7) | x <- [0 .. 0xffff `div` 8]]
    return $ ["RAM", ""] ++ ls ++ [""]
  where
    line (lo, up) = do
        mem  <- ask
        vals <- mapM (lift . Memory.load mem . Memory.ram) [lo .. up]
        return $ prettifyWord16 lo ++ ": " ++ unwords (map prettifyWord16 vals)
