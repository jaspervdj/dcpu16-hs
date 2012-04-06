module Assembler
    ( parse
    , calculateLabels
    , assemble
    ) where

import Data.List (foldl')
import Data.Map (Map)
import Data.Word (Word16)
import qualified Data.Map as M

import qualified Text.Parsec as P

import Assembler.Parser
import Instruction

parse :: FilePath -> String -> [(Maybe Label, Instruction AValue)]
parse filePath source = case P.parse statements filePath source of
    Left err -> error $ show err
    Right xs -> xs

-- | Length of an instruction (in words)
instructionLength :: Instruction AValue -> Int
instructionLength instruction = case instruction of
    BasicInstruction _ a b  -> 1 + valueLength a + valueLength b
    NonBasicInstruction _ a -> 1 + valueLength a
  where
    -- Extra words needed to encode value
    valueLength (ALiteral _)                = 1
    valueLength (APLiteral _)               = 1
    valueLength (APLiteralPlusRegister _ _) = 1
    valueLength (ALabel _)                  = 1
    valueLength _                           = 0

calculateLabels :: [(Maybe Label, Instruction AValue)] -> Map Label Int
calculateLabels = snd . foldl' step (0, M.empty)
  where
    step (i, labels) (label, instr) = case label of
        Nothing                   -> (i', labels)
        Just l
            | l `M.member` labels -> error $ "Duplicate label: " ++ l
            | otherwise           -> (i', M.insert l i labels)
      where
        i' = i + instructionLength instr

makeOperand :: Map Label Int -> AValue -> (Operand, [Word16])
makeOperand labels val = case val of
    (ARegister r)               -> (ORegister r, [])
    (APRegister r)              -> (OPRegister r, [])
    (ALiteral w)                -> (ONextWord, [w])
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
