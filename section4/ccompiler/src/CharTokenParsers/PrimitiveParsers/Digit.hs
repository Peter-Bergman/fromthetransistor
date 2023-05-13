module CharTokenParsers.PrimitiveParsers.Digit (digit) where
import AbstractSyntaxTree
import CustomCombinators
import Text.Parsec
    (parserFail)
import qualified Text.Parsec.Char as Char
import Text.Parsec.String
import qualified Text.Read as Read


digit :: Parser Digit
digit = tryWithFailMessage "Digit" $ do
    parsedDigitChar <- Char.digit
    let maybeDigit = Read.readMaybe (parsedDigitChar : [])
    case maybeDigit of
        Just digit' -> return $ Digit digit'
        Nothing     -> parserFail "error in digit parser"

