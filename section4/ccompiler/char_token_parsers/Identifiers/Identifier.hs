module CharTokenParsers.Identifiers.Identifier where
import CharTokenParsers.PrimitiveParsers.UniversalCharacterName
import Data.List
import Numeric
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String


identifier :: Parser String
identifier = (do
    let digitString = do
        digitParsed <- digit
        return [digitParsed]
    firstCharacter <- identifierNonDigit
    remainingCharacters <- many (digitString <|> identifierNonDigit)
    let identifier = firstCharacter ++ (intercalate "" remainingCharacters)
    return identifier) <?> "Identifier"


identifierNonDigit :: Parser String
identifierNonDigit = nonDigit <|> universalCharacterName

nonDigit :: Parser String
nonDigit = do
    character <- letter <|> char '_'
    return [character]



