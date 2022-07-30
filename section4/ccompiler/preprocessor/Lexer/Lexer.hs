module Lexer where
import Data.List
import Text.Parsec hiding (try)
import Text.ParserCombinators.Parsec
import PreprocessingToken
 
deleteBackSlashedNewLines :: Parser Char
deleteBackSlashedNewLines = do
    parsedStrings <- many1 ((try (string "\\\n") >> return ""))
    let parsedStringsFlattened = intercalate "" parsedStrings
    -- Technically, the line below is an oversimplification.
    -- Only NEED to append a newline if the source does not already end with one.
    -- Adding either way for simplicity
    let newLinedString = parsedStringsFlattened ++ "\n"
    --return newLinedString
    return ' '


removeString :: String -> Parser String
removeString inputString = do
    parsedString <- try (string inputString) <|> anyCharAsString
    if parsedString == inputString
        then return ""
        else return inputString

anyCharAsString :: Parser String
anyCharAsString = do
    parsedChar <- anyChar
    return $ parsedChar : ""

spacingCompressionLexer :: Parser String
spacingCompressionLexer = (do
    parsedSpaces <- skipMany1 (((char ' ') <|> (char '\t') <|> (try deleteBackSlashedNewLines)) <?> "Spacing")
    return " ") <?> ""
    

lexC :: Parser [String]
lexC = do
    tokenList <- many $ spacingCompressionLexer <|> preprocessingToken
    return tokenList

