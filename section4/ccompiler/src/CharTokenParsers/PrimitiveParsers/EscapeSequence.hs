module CharTokenParsers.PrimitiveParsers.EscapeSequence where
import AbstractSyntaxTree
import CustomCombinators
import CharTokenParsers.PrimitiveParsers.OctalDigit
import CharTokenParsers.PrimitiveParsers.UniversalCharacterName
import qualified Data.List.NonEmpty
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String




{-escapeSequence :: Parser String
escapeSequence = 
    simpleEscapeSequence <|>
    octalEscapeSequence <|>
    hexadecimalEscapeSequence <|>
    universalCharacterName

simpleEscapeSequence :: Parser String
simpleEscapeSequence = backSlashed $ oneOf "'\"?\\abfnrtv"
    where
        backSlashed :: Parser Char -> Parser String
        backSlashed input = do
            char '\\'
            parsedInput <- input
            return $ '\\' : [parsedInput]



octalEscapeSequence :: Parser String
octalEscapeSequence = do
    {- 
       Note that on the last tried parser in octalDigitSequence,
       we have to use the count parser combinator instead of 
       simply the octDigit parser because octDigit is a
       Char parser.
       octalDigitSequence should implicitly be a String Parser
    -}
    backSlash <- char '\\'
    octalDigits <- octalDigitSequence
    return $ backSlash : octalDigits
    where
        octalDigitSequence = try (count 3 octDigit) <|>
            try (count 2 octDigit) <|>
            (count 1 octDigit)

hexadecimalEscapeSequence :: Parser String
hexadecimalEscapeSequence = do
    prefix <- string "\\x"
    hexadecimalDigits <- many1 hexDigit
    return $ prefix ++ hexadecimalDigits
-}

escapeSequence :: Parser EscapeSequence
escapeSequence =
    simpleEscapeSequenceEscapeSequence <|>
    octalEscapeSequenceEscapeSequence <|>
    hexadecimalEscapeSequenceEscapeSequence <|>
    universalCharacterNameEscapeSequence

simpleEscapeSequenceEscapeSequence :: Parser EscapeSequence
simpleEscapeSequenceEscapeSequence = SimpleEscapeSequenceEscapeSequence <$> simpleEscapeSequence

simpleEscapeSequence :: Parser SimpleEscapeSequence
simpleEscapeSequence = SimpleEscapeSequence <$> anyOf 
    [ string "\\\'"
    , string "\\\""
    , string "\\?"
    , string "\\\\"
    , string "\\a"
    , string "\\b"
    , string "\\f"
    , string "\\n"
    , string "\\r"
    , string "\\t"
    , string "\\v"
    ]

octalEscapeSequenceEscapeSequence :: Parser EscapeSequence
octalEscapeSequenceEscapeSequence = simpleExpression octalEscapeSequence OctalEscapeSequenceEscapeSequence

octalEscapeSequence :: Parser OctalEscapeSequence
octalEscapeSequence =
    tripleOctalEscapeSequence <|>
    doubleOctalEscapeSequence <|>
    singleOctalEscapeSequence

singleOctalEscapeSequence :: Parser OctalEscapeSequence
singleOctalEscapeSequence = try $ simpleExpression (char '\\' >> octalDigit) SingleOctalEscapeSequence

doubleOctalEscapeSequence :: Parser OctalEscapeSequence
doubleOctalEscapeSequence = try $ do
    char '\\'
    parsedFirstOctalDigit <- octalDigit
    parsedSecondOctalDigit <- octalDigit
    return $ DoubleOctalEscapeSequence parsedFirstOctalDigit parsedSecondOctalDigit

tripleOctalEscapeSequence :: Parser OctalEscapeSequence
tripleOctalEscapeSequence = try $ do
    char '\\'
    parsedFirstOctalDigit <- octalDigit
    parsedSecondOctalDigit <- octalDigit
    parsedThirdOctalDigit <- octalDigit
    return $ TripleOctalEscapeSequence parsedFirstOctalDigit parsedSecondOctalDigit parsedThirdOctalDigit

hexadecimalEscapeSequenceEscapeSequence :: Parser EscapeSequence
hexadecimalEscapeSequenceEscapeSequence = HexadecimalEscapeSequenceEscapeSequence <$> hexadecimalEscapeSequence

hexadecimalEscapeSequence :: Parser HexadecimalEscapeSequence
hexadecimalEscapeSequence = do
    parsedPrefix <- string "\\x"
    parsedHexadecimalDigitChars <- many1NonEmpty hexDigit
    let parsedHexadecimalDigits = Data.List.NonEmpty.map HexadecimalDigit parsedHexadecimalDigitChars
    return $ HexadecimalEscapeSequence parsedHexadecimalDigits

universalCharacterNameEscapeSequence :: Parser EscapeSequence
universalCharacterNameEscapeSequence = UniversalCharacterNameEscapeSequence <$> universalCharacterName

