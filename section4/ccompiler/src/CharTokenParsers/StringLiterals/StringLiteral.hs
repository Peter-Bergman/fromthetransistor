module CharTokenParsers.StringLiterals.StringLiteral where
import AbstractSyntaxTree
import CharTokenParsers.PrimitiveParsers.EscapeSequence
import CustomCombinators
import Data.List
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String

stringLiteral :: Parser StringLiteral
stringLiteral = tryWithFailMessage "String Literal" $ do
    parsedMaybeEncodingPrefix <- tryMaybe encodingPrefix
    _ <- char '"'
    parsedMaybeSCharSequence <- tryMaybe sCharSequence
    _ <- char '"'
    return $ StringLiteral parsedMaybeEncodingPrefix parsedMaybeSCharSequence
    

quoted :: String -> String
quoted input = "\"" ++ input ++ "\""


encodingPrefix :: Parser EncodingPrefix
encodingPrefix =
    u8 <|>
    lowerCaseU <|>
    capitalU <|>
    capitalL
{- 
    try (string "u8") <|>
    string "u" <|>
    string "U" <|>
    string "L"
-}

u8 :: Parser EncodingPrefix
u8 = try $ string "u8" >> return U8

lowerCaseU :: Parser EncodingPrefix
lowerCaseU = parserFail "lowerCaseU parser not implemented yet"

capitalU :: Parser EncodingPrefix
capitalU = parserFail "capitalU parser not implemented yet"

capitalL :: Parser EncodingPrefix
capitalL = parserFail "capitalL parser not implemented yet"

sCharSequence :: Parser SCharSequence
sCharSequence = simpleExpression (many1NonEmpty sChar) SCharSequence
{-do
    sChars <- many1 sChar
    let sCharsString = intercalate "" sChars
    return sCharsString
-}

sChar :: Parser SChar
sChar = 
    nonEscapeSequenceSChar <|>
    escapeSequenceSChar

nonEscapeSequenceSChar :: Parser SChar
nonEscapeSequenceSChar = simpleExpression (noneOf forbiddenCharacters) NonEscapeSequenceSChar

escapeSequenceSChar :: Parser SChar
escapeSequenceSChar = simpleExpression escapeSequence EscapeSequenceSChar

forbiddenCharacters :: String
forbiddenCharacters = '\"' : '\\' : '\n' : ""

{-noneOfString :: String -> Parser String
noneOfString unallowedChars = do
    parsedCharacter <- noneOf unallowedChars
    return $ parsedCharacter : ""
-}
