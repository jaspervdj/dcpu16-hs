module Assembler.Parser
    ( Label
    , AValue (..)
    , statements
    ) where

import Control.Applicative ((<$>), (<*>), (<|>), (<*), (*>))
import Control.Monad (void)
import Data.Char (isSpace)
import Data.Word (Word16)

import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

import Instruction
import Memory (Register (..))

type Label = String

data AValue
    = ARegister Register
    | APRegister Register
    | ALiteral Word16
    | APLiteral Word16
    | APLiteralPlusRegister Word16 Register
    | APop
    | APeek
    | APush
    | ASp
    | APc
    | AO
    | ALabel Label
    deriving (Show)

statements :: Parser [(Maybe Label, Instruction AValue)]
statements = P.many statement

statement :: Parser (Maybe Label, Instruction AValue)
statement = do
    P.skipMany $ P.choice [void (P.space), void comment]
    label'       <- P.option Nothing $ fmap Just label
    P.skipMany $ P.choice [void (P.space), void comment]
    instruction' <- instruction
    P.skipMany $ P.choice [void (P.space), void comment]
    return (label', instruction')

comment :: Parser String
comment = (:)
    <$> P.char ';'
    <*> P.manyTill P.anyChar (void P.newline <|> P.eof)

label :: Parser Label
label = P.char ':' *> until1 isSpace

instruction :: Parser (Instruction AValue)
instruction = basicInstruction <|> nonBasicInstruction

basicInstruction :: Parser (Instruction AValue)
basicInstruction = do
    op <- P.choice $ map tryString instructions
    P.skipMany1 P.space
    a <- value
    P.skipMany P.space
    _ <- P.char ','
    P.skipMany P.space
    b <- value
    return $ BasicInstruction op a b
  where
    instructions =
        [ ("SET", Set)
        , ("ADD", Add)
        , ("SUB", Sub)
        , ("MUL", Mul)
        , ("DIV", Div)
        , ("MOD", Mod)
        , ("SHL", Shl)
        , ("SHR", Shr)
        , ("AND", And)
        , ("BOR", Bor)
        , ("XOR", Xor)
        , ("IFE", Ife)
        , ("IFN", Ifn)
        , ("IFG", Ifg)
        , ("IFB", Ifb)
        ]

nonBasicInstruction :: Parser (Instruction AValue)
nonBasicInstruction = do
    op <- P.choice $ map tryString instructions
    P.skipMany1 P.space
    a <- value
    return $ NonBasicInstruction op a
  where
    instructions =
        [ ("JSR", Jsr)
        ]

register :: Parser Register
register = P.choice $ map tryString
    [ ("A", A)
    , ("B", B)
    , ("C", C)
    , ("X", X)
    , ("Y", Y)
    , ("Z", Z)
    , ("I", I)
    , ("J", J)
    ]

literal :: Parser Word16
literal = P.try hexadecimal <|> P.try decimal
  where
    hexadecimal = do
        _  <- P.string "0x"
        xs <- P.many1 P.hexDigit
        return $ read $ "0x" ++ xs
    decimal = do
        xs <- P.many1 P.digit
        return $ read xs

value :: Parser AValue
value = P.choice $ map P.try
    [ ARegister             <$> register
    , APRegister            <$> brackets register
    , ALiteral              <$> literal
    , APLiteral             <$> brackets literal
    , brackets $ APLiteralPlusRegister
        <$> literal
        <*  P.skipMany P.space
        <*  P.char '+'
        <*  P.skipMany P.space
        <*> register
    , P.string "POP"  *> return APop
    , P.string "PEEK" *> return APeek
    , P.string "PUSH" *> return APush
    , P.string "SP"   *> return ASp
    , P.string "PC"   *> return APc
    , P.string "O"    *> return AO
    , ALabel <$> until1 isSpace
    ]
  where
    brackets parser = do
        _ <- P.char '['
        P.skipMany P.space
        x <- parser
        P.skipMany P.space
        _ <- P.char ']'
        return x

until1 :: (Char -> Bool) -> Parser String
until1 = P.many1 . P.satisfy . (not .)

tryString :: (String, a) -> Parser a
tryString (str, x) = P.try (P.string str) *> return x
