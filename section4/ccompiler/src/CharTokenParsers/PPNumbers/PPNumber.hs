module CharTokenParsers.PPNumbers.PPNumber (ppNumber) where
import AbstractSyntaxTree
import CharTokenParsers.PrimitiveParsers.UniversalCharacterName
import CustomCombinators
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

ppNumberSuffix :: Parser PPNumberSuffix
ppNumberSuffix = 
    digitPPNumberSuffix <|>
    identifierNonDigitPPNumberSuffix <|>
    lowerCaseEPPNumberSuffix <|>
    capitalEPPNumberSuffix <|>
    lowerCasePPPNumberSuffix <|>
    capitalPPPNumberSuffix <|>
    dotPPNumberSuffix

digitPPNumberSuffix :: Parser PPNumberSuffix
digitPPNumberSuffix = simpleExpression digit Digit

identifierNonDigitPPNumberSuffix :: Parser PPNumberSuffix
identifierNonDigitPPNumberSuffix = simpleExpression identifierNonDigit IdentifierNonDigitPPNumberSuffix

lowerCaseEPPNumberSuffix :: Parser PPNumberSuffix
lowerCaseEPPNumberSuffix = try $ char 'e' >> simpleExpression sign LowerCaseEPPNumberSuffix

capitalEPPNumberSuffix :: Parser PPNumberSuffix
capitalEPPNumberSuffix = try $ char 'E' >> simpleExpression sign CapitalEPPNumberSuffix

identifierNonDigit :: Parser IdentifierNonDigit
identifierNonDigit = universalCharacterNameNonDigit <|> nonDigitIdentifierNonDigit

universalCharacterNameNonDigit :: Parser IdentifierNonDigit
universalCharacterNameNonDigit = simpleExpression universalCharacterName UniversalCharacterNameNonDigit

nonDigitIdentifierNonDigit :: Parser IdentifierNonDigit
nonDigitIdentifierNonDigit = simpleExpression nonDigit NonDigitIdentifierNonDigit

{-nonDigitAsString :: Parser String
nonDigitAsString = nonDigit >>= return . (:"")
-}

nonDigit :: Parser NonDigit
nonDigit = simpleExpression (letter <|> char '_') NonDigit

littleESign = charThenSign 'e'
bigESign = charThenSign 'E'
littlePSign = charThenSign 'p'
bigPSign = charThenSign 'P'

sign :: Parser Sign
sign = minusSign <|> plusSign

minusSign :: Parser Sign
minusSign = char '-' >> return MinusSign

plusSign :: Parser Sign
plusSign = char '+' >> return PlusSign

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

