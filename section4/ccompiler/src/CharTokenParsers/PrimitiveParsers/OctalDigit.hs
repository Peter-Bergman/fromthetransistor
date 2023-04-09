module CharTokenParsers.PrimitiveParsers.OctalDigit where
import AbstractSyntaxTree
import CustomCombinators
    (simpleExpression)
import Text.Parsec.Char
    (octDigit)
import Text.Parsec.String
    (Parser)

octalDigit :: Parser OctalDigit
octalDigit = OctalDigit . read . (: []) <$> octDigit

