-- | Simple prettified log
module Emulator.Log
    ( prettify
    , state
    , core
    , registers
    , ram
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM)
import Data.List (intercalate)

import Emulator
import Emulator.Monad
import Instruction
import Memory (Address (..))
import Util

prettify :: MonadEmulator m => m String
prettify = unlines <$> sequence
    [core , registers , return "" , return "RAM:" , return "" , ram]

state :: MonadEmulator m => Instruction Operand -> Instruction Value -> m String
state instr instr' = unlines <$> sequence
    [ return $ "Execute: " ++ show instr ++ " -> " ++ show instr'
    , core
    , registers
    ]

core :: MonadEmulator m => m String
core = do
    pc     <- load Pc
    sp     <- load Sp
    o      <- load O
    cycles <- load Cycles
    return $ intercalate ", " $
        [ "PC: "     ++ prettifyWord16 pc
        , "SP: "     ++ prettifyWord16 sp
        , "O: "      ++ prettifyWord16 o
        , "CYCLES: " ++ prettifyWord16 cycles
        ]

registers :: MonadEmulator m => m String
registers = do
    rs <- forM [minBound .. maxBound] $ \name -> do
        val <- load (Register name)
        return (name, val)
    return $ intercalate ", " $
        [show name ++ ": " ++ prettifyWord16 val | (name, val) <- rs]

ram :: MonadEmulator m => m String
ram = unlines <$> mapM line [(x * 8, x * 8 + 7) | x <- [0 .. 0xffff `div` 8]]
  where
    line (lo, up) = do
        vs <- mapM (load . Ram) [lo .. up]
        return $ prettifyWord16 lo ++ ": " ++ unwords (map prettifyWord16 vs)
