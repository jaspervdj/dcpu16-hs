module Assembler
    ( assembleFile
    , parse
    , calculateLabels
    , instructions
    , assemble
    ) where

import Data.List (foldl')
import Data.Map (Map)
import Data.Word (Word16)
import qualified Data.Map as M

import qualified Blaze.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Text.Parsec as P

import Assembler.Parser
import Instruction

-- | Very high level
assembleFile :: FilePath -> FilePath -> IO ()
assembleFile fileIn fileOut = do
    source <- readFile fileIn
    let sts    = parse fileIn source
        labels = calculateLabels sts
        w16s   = assemble labels $ instructions sts
    BL.writeFile fileOut $ B.toLazyByteString $ B.fromWord16sbe w16s

parse :: FilePath -> String -> [Statement]
parse filePath source = case P.parse statements filePath source of
    Left err -> error $ show err
    Right xs -> xs

-- | Can a value be used in short form?
shortForm :: Word16 -> Bool
shortForm = (<= 0x1f)

-- | Exact length of an instruction (in words)
instructionLength :: Instruction AValue -> Int
instructionLength instruction = case instruction of
    BasicInstruction _ a b  -> 1 + valueLength a + valueLength b
    NonBasicInstruction _ a -> 1 + valueLength a
  where
    -- Extra words needed to encode value
    valueLength (ALiteral w)                = if shortForm w then 0 else 1
    valueLength (APLiteral _)               = 1
    valueLength (APLiteralPlusRegister _ _) = 1
    valueLength (ALabel _)                  = 1
    valueLength _                           = 0

calculateLabels :: [Statement] -> Map Label Int
calculateLabels = snd . foldl' step (0, M.empty)
  where
    step (i, ls) statement = case statement of
        Instruction instr     -> (i + instructionLength instr, ls)
        Label l
            | l `M.member` ls -> error $ "Duplicate label: " ++ l
            | otherwise       -> (i, M.insert l i ls)

instructions :: [Statement] -> [Instruction AValue]
instructions sts = [instr | Instruction instr <- sts]

makeOperand :: Map Label Int -> AValue -> (Operand, [Word16])
makeOperand labels val = case val of
    (ARegister r)               -> (ORegister r, [])
    (APRegister r)              -> (OPRegister r, [])
    (ALiteral w)                -> nextWordOrLiteral w
    (APLiteral w)               -> (OPNextWord, [w])
    (APLiteralPlusRegister w r) -> (OPNextWordPlusRegister r, [w])
    APop                        -> (OPop, [])
    APeek                       -> (OPeek, [])
    APush                       -> (OPush, [])
    ASp                         -> (OSp, [])
    APc                         -> (OPc, [])
    AO                          -> (OO, [])
    (ALabel l)                  -> (ONextWord, [findLabel l])
  where
    findLabel l = case M.lookup l labels of
        Nothing -> error $ "Unknown label: " ++ l
        Just i  -> fromIntegral i

    -- Either put the literal in the next word, or embed it if it's small enough
    nextWordOrLiteral w
        | shortForm w = (OLiteral w, [])
        | otherwise   = (ONextWord, [w])

assembleInstruction :: Map Label Int
                    -> Instruction AValue
                    -> [Word16]
assembleInstruction labels instruction = case instruction of
    (BasicInstruction op a b)  ->
        let (oa, w1) = makeOperand labels a
            (ob, w2) = makeOperand labels b
        in [encodeInstruction (BasicInstruction op oa ob)] ++ w1 ++ w2
    (NonBasicInstruction op a) ->
        let (oa, w1) = makeOperand labels a
        in [encodeInstruction (NonBasicInstruction op oa)] ++ w1

assemble :: Map Label Int -> [Instruction AValue] -> [Word16]
assemble labels = concatMap (assembleInstruction labels)
