module Identifiers.Identifier where
import Data.List
import Numeric
import PrimitiveParsers.UniversalCharacterName
import Text.Parsec.Char
import Text.ParserCombinators.Parsec


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



