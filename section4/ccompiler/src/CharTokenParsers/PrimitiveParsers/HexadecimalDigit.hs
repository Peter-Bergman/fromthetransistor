module CharTokenParsers.PrimitiveParsers.HexadecimalDigit (hexadecimalDigit) where
import Text.Parsec.Char
    (hexDigit)

hexadecimalDigit :: Parser HexadecimalDigit
hexadecimalDigit = HexadecimalDigit <$> hexDigit

