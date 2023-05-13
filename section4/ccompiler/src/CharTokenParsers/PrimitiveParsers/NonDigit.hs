module CharTokenParsers.PrimitiveParsers.NonDigit (nonDigit) where
import AbstractSyntaxTree
import CustomCombinators
import Text.Parsec.Char as Char
import Text.Parsec.Prim
import Text.Parsec.String

nonDigit :: Parser NonDigit
nonDigit = simpleExpression (Char.letter <|> Char.char '_') NonDigit

