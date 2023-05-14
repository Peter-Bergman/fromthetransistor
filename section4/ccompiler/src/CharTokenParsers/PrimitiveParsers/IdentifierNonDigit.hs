module CharTokenParsers.PrimitiveParsers.IdentifierNonDigit (identifierNonDigit) where
import AbstractSyntaxTree
import CustomCombinators
import CharTokenParsers.PrimitiveParsers.NonDigit
import CharTokenParsers.PrimitiveParsers.UniversalCharacterName
import Text.Parsec
    ((<|>))
import Text.Parsec.String

identifierNonDigit :: Parser IdentifierNonDigit
identifierNonDigit = universalCharacterNameIdentifierNonDigit <|> nonDigitIdentifierNonDigit

universalCharacterNameIdentifierNonDigit :: Parser IdentifierNonDigit
universalCharacterNameIdentifierNonDigit = simpleExpression universalCharacterName UniversalCharacterNameIdentifierNonDigit

nonDigitIdentifierNonDigit :: Parser IdentifierNonDigit
nonDigitIdentifierNonDigit = simpleExpression nonDigit NonDigitIdentifierNonDigit

