module StringLiterals.StringLiteral where
import Data.List
import PrimitiveParsers.EscapeSequence
import Text.ParserCombinators.Parsec
import Text.Parsec hiding (try)

stringLiteral :: Parser String
stringLiteral = do
    prefix <- option "" encodingPrefix
    char '"'
    stringCharacterSequence <- option "" sCharSequence
    char '"'
    return $ prefix ++ (quoted stringCharacterSequence)
    

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

