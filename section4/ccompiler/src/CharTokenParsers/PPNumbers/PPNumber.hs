module CharTokenParsers.PPNumbers.PPNumber (ppNumber) where
import CharTokenParsers.PrimitiveParsers.UniversalCharacterName
import Data.List
import System.Environment
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

ppNumber :: Parser String
ppNumber = (do
    parsedDot <- option "" dotAsString
    initialDigit <- digit
    tail <- many ppNumberSuffix
    let tailString = intercalate "" tail
    return $ parsedDot ++ initialDigit : tailString) <?> "Preprocessing Number"

ppNumberSuffix :: Parser String
ppNumberSuffix = 
    digitAsString <|>
    identifierDigit <|>
    littleESign <|>
    bigESign <|>
    littlePSign <|>
    bigPSign <|>
    dotAsString

identifierDigit :: Parser String
identifierDigit = universalCharacterName <|> nonDigitAsString

nonDigitAsString :: Parser String
nonDigitAsString = nonDigit >>= return . (:"")

nonDigit :: Parser Char
nonDigit = letter <|> char '_'

littleESign = charThenSign 'e'
bigESign = charThenSign 'E'
littlePSign = charThenSign 'p'
bigPSign = charThenSign 'P'

sign :: Parser String
sign = string "-" <|> string "+"

charThenSign :: Char -> Parser String
charThenSign character = do
    parsedCharacter <- char character
    sign <- sign
    return $ parsedCharacter : sign

dotAsString :: Parser String
dotAsString = do
    char '.'
    return "."

digitAsString :: Parser String
digitAsString = do
    parsedDigit <- digit
    return $ parsedDigit : ""

