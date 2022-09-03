module CharTokenParsers.StringLiterals.StringLiteral where
import CharTokenParsers.PrimitiveParsers.EscapeSequence
import Data.List
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String

stringLiteral :: Parser String
stringLiteral = (do
    prefix <- option "" encodingPrefix
    char '"'
    stringCharacterSequence <- option "" sCharSequence
    char '"'
    return $ prefix ++ (quoted stringCharacterSequence)) <?> "String Literal"
    

quoted :: String -> String
quoted input = "\"" ++ input ++ "\""


encodingPrefix :: Parser String
encodingPrefix = 
    try (string "u8") <|>
    string "u" <|>
    string "U" <|>
    string "L"

sCharSequence :: Parser String
sCharSequence = do
    sChars <- many1 sChar
    let sCharsString = intercalate "" sChars
    return sCharsString

sChar :: Parser String
sChar = 
    noneOfString forbiddenCharacters <|>
    escapeSequence


forbiddenCharacters :: String
forbiddenCharacters = '\"' : '\\' : '\n' : ""

noneOfString :: String -> Parser String
noneOfString unallowedChars = do
    parsedCharacter <- noneOf unallowedChars
    return $ parsedCharacter : ""

