module CharTokenParsers.Constants.EnumerationConstants.EnumerationConstant (enumerationConstant) where
import AbstractSyntaxTree
import CharTokenParsers.Identifiers.Identifier
    (identifier)
import CustomCombinators
    (simpleExpression)
import Text.Parsec.String
    (Parser)

enumerationConstant :: Parser EnumerationConstant
enumerationConstant = simpleExpression identifier EnumerationConstant


