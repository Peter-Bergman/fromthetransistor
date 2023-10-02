module CharTokenParsers.PrimitiveParsers.EscapeSequence where
import AbstractSyntaxTree
import CustomCombinators
import CharTokenParsers.PrimitiveParsers.OctalDigit
import CharTokenParsers.PrimitiveParsers.UniversalCharacterName
import qualified Data.List.NonEmpty
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String


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
    _ <- char '\\'
    parsedFirstOctalDigit <- octalDigit
    parsedSecondOctalDigit <- octalDigit
    return $ DoubleOctalEscapeSequence parsedFirstOctalDigit parsedSecondOctalDigit

tripleOctalEscapeSequence :: Parser OctalEscapeSequence
tripleOctalEscapeSequence = try $ do
    _ <- char '\\'
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

