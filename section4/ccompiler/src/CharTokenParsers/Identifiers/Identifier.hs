module CharTokenParsers.Identifiers.Identifier (identifier) where
import CharTokenParsers.PrimitiveParsers.UniversalCharacterName
import CustomCombinators
    ( singleton
    , tryWithFailMessage
    )
import Data.List
import Numeric
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String


identifier :: Parser String
identifier = tryWithFailMessage "Identifier" do
    firstCharacter <- identifierNonDigit
    remainingCharacters <- many (digitString <|> identifierNonDigit)
    let identifier = firstCharacter ++ (intercalate "" remainingCharacters)
    return identifier

digitString :: Parser String
digitString = singleton digit

identifierNonDigit :: Parser String
identifierNonDigit = nonDigit <|> universalCharacterName

nonDigit :: Parser String
nonDigit = do
    character <- letter <|> char '_'
    return [character]



