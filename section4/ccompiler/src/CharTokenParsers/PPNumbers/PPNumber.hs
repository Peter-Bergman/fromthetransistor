module CharTokenParsers.PPNumbers.PPNumber (ppNumber) where
import AbstractSyntaxTree
import CharTokenParsers.PrimitiveParsers.Digit
import CharTokenParsers.PrimitiveParsers.NonDigit
import CharTokenParsers.PrimitiveParsers.UniversalCharacterName
import CustomCombinators
import Data.List
import qualified Text.Parsec.Char as Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String
import qualified Text.Read as Read

ppNumber :: Parser PPNumber
ppNumber = tryWithFailMessage "PPNumber" $ dottedDigitPPNumber <|> digitPPNumber

dottedDigitPPNumber :: Parser PPNumber
dottedDigitPPNumber = tryWithFailMessage "Dotted Digit PPNumber" $ do
    Char.char '.'
    parsedDigit <- digit
    parsedPPNumberSuffixes <- many ppNumberSuffix
    return $ DottedDigitPPNumber parsedDigit parsedPPNumberSuffixes

digitPPNumber :: Parser PPNumber
digitPPNumber = tryWithFailMessage "Digit PPNumber" $ do
    parsedDigit <- digit
    parsedPPNumberSuffixes <- many ppNumberSuffix
    return $ DigitPPNumber parsedDigit parsedPPNumberSuffixes

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
digitPPNumberSuffix = simpleExpression digit DigitPPNumberSuffix

identifierNonDigitPPNumberSuffix :: Parser PPNumberSuffix
identifierNonDigitPPNumberSuffix = simpleExpression identifierNonDigit IdentifierNonDigitPPNumberSuffix

lowerCaseEPPNumberSuffix :: Parser PPNumberSuffix
lowerCaseEPPNumberSuffix = try $ Char.char 'e' >> simpleExpression sign LowerCaseEPPNumberSuffix

capitalEPPNumberSuffix :: Parser PPNumberSuffix
capitalEPPNumberSuffix = try $ Char.char 'E' >> simpleExpression sign CapitalEPPNumberSuffix

lowerCasePPPNumberSuffix :: Parser PPNumberSuffix
lowerCasePPPNumberSuffix = try $ Char.char 'p' >> simpleExpression sign LowerCasePPPNumberSuffix

capitalPPPNumberSuffix :: Parser PPNumberSuffix
capitalPPPNumberSuffix = try $ Char.char 'P' >> simpleExpression sign CapitalPPPNumberSuffix

dotPPNumberSuffix :: Parser PPNumberSuffix
dotPPNumberSuffix = Char.char '.' >> return DotPPNumberSuffix

identifierNonDigit :: Parser IdentifierNonDigit
identifierNonDigit = universalCharacterNameNonDigit <|> nonDigitIdentifierNonDigit

universalCharacterNameNonDigit :: Parser IdentifierNonDigit
universalCharacterNameNonDigit = simpleExpression universalCharacterName UniversalCharacterNameNonDigit

nonDigitIdentifierNonDigit :: Parser IdentifierNonDigit
nonDigitIdentifierNonDigit = simpleExpression nonDigit NonDigitIdentifierNonDigit

sign :: Parser Sign
sign = minusSign <|> plusSign

minusSign :: Parser Sign
minusSign = Char.char '-' >> return MinusSign

plusSign :: Parser Sign
plusSign = Char.char '+' >> return PlusSign

