module Constants.CharacterConstants.CharacterConstant where
import Data.List
import Numeric
import PrimitiveParsers.UniversalCharacterName
import PrimitiveParsers.EscapeSequence
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Text

characterConstant :: Parser String
characterConstant = (do
    prefix <- characterPrefix
    openingQuote <- char '\''
    parsedCCharSequence <- cCharSequence
    closingQuote <- char '\'' <?> "closing quote"
    return $ prefix ++ [openingQuote] ++  parsedCCharSequence ++ [closingQuote]) <?> "Character Constant"


characterPrefix :: Parser String
characterPrefix = do
    -- 'a' is just an arbitrary character here
    -- Any non 'L', 'u', or 'U' character would work in its place.
    let alternativeCharacter = 'a'
    let optionalPrefix = option alternativeCharacter (oneOf "LuU")
    prefixChar <- optionalPrefix
    let parsedPrefix = if prefixChar == alternativeCharacter
        then ""
        else [prefixChar]
    return parsedPrefix


singleQuoted :: Parser String -> Parser String
singleQuoted = between (string "'") (string "'")

cCharSequence :: Parser String
cCharSequence = do
    firstCharacter <- cChar
    restOfCharactersList <- many cChar
    let restOfCharacters = intercalate "" restOfCharactersList
    return $ firstCharacter ++ restOfCharacters

cChar :: Parser String
cChar = nonEscapeSequence <|> escapeSequence

nonEscapeSequence :: Parser String
nonEscapeSequence = do
    parsedCharacter <- noneOf $ '\'' : '\\' : "\n"
    return [parsedCharacter]

