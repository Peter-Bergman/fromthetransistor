module CharTokenParsers.PrimitiveParsers.HexadecimalDigit (hexadecimalDigit) where
import AbstractSyntaxTree
    ( HexadecimalDigit
        (HexadecimalDigit)
    )
import Text.Parsec.Char
    (hexDigit)
import Text.Parsec.String
    (Parser)

hexadecimalDigit :: Parser HexadecimalDigit
hexadecimalDigit = HexadecimalDigit <$> hexDigit

