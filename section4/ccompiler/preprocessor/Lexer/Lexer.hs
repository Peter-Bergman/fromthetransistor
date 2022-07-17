module Lexer where
import Data.List
import Text.Parsec hiding (try)
import Text.ParserCombinators.Parsec
import PreprocessingToken
 
deleteBackSlashedNewLines :: Parser String
deleteBackSlashedNewLines = do
    parsedStrings <- many ((try (string "\\\n") >> return "") <|> anyCharAsString)
    let parsedStringsFlattened = intercalate "" parsedStrings
    -- Technically, the line below is an oversimplification.
    -- Only NEED to append a newline if the source does not already end with one.
    -- Adding either way for simplicity
    let newLinedString = parsedStringsFlattened ++ "\n"
    return newLinedString


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

spacingCompressedLexer :: Parser String
spacingCompressedLexer = do
    parsedSpaces <- many ((char ' ') <|> (char ' '))
    return " "
    


lexC :: Parser [String]
lexC = do
    tokenList <- many (spaces >> preprocessingToken)
    spaces
    return tokenList
    
