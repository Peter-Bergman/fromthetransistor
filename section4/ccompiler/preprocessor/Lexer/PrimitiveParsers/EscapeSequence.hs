module PrimitiveParsers.EscapeSequence where
import PrimitiveParsers.UniversalCharacterName
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Text




escapeSequence :: Parser String
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


