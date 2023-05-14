module CharTokenParsers.Identifiers.Identifier (identifier) where
import AbstractSyntaxTree
import CharTokenParsers.PrimitiveParsers.Digit
import CharTokenParsers.PrimitiveParsers.IdentifierNonDigit
import CharTokenParsers.PrimitiveParsers.NonDigit
import CharTokenParsers.PrimitiveParsers.UniversalCharacterName
import CustomCombinators
import Numeric
import Text.Parsec
    ( (<|>)
    , try
    , many
    )
import qualified Text.Parsec.Char as Char
import Text.Parsec.String


identifier :: Parser Identifier
identifier = tryWithFailMessage "Identifier" $ do
    parsedIdentifierNonDigit <- identifierNonDigit
    parsedIdentifierSuffixes <- many identifierSuffix
    return $ Identifier parsedIdentifierNonDigit parsedIdentifierSuffixes

identifierSuffix :: Parser IdentifierSuffix
identifierSuffix = try $ identifierNonDigitIdentifierSuffix <|> digitIdentifierSuffix

identifierNonDigitIdentifierSuffix :: Parser IdentifierSuffix
identifierNonDigitIdentifierSuffix = simpleExpression identifierNonDigit IdentifierNonDigitIdentifierSuffix

digitIdentifierSuffix :: Parser IdentifierSuffix
digitIdentifierSuffix = simpleExpression digit DigitIdentifierSuffix

