module Lexer.Lexer where
import Data.List
import Lexer.PreprocessingToken
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String
import System.Environment

-- change name to lexBackSlashedNewLinesAsSpace 
lexBackSlashedNewLinesAsSpace :: Parser Char
lexBackSlashedNewLinesAsSpace = do
    parsedStrings <- many1 ((try (string "\\\n") >> return ""))
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
    parsedSpaces <- skipMany1 (((char ' ') <|> (char '\t') <|> (try lexBackSlashedNewLinesAsSpace)) <?> "Spacing")
    return " ") <?> ""
    
newLineAsString :: Parser String
newLineAsString = do
    parsedNewLineCharacter <- char '\n'
    return [ parsedNewLineCharacter ]

lexC :: Parser [String]
lexC = do
    tokenList <- many $ spacingCompressionLexer <|> preprocessingToken <|> newLineAsString
    let filteredTokenList = filter ( \token -> token `notElem` ["", " "] ) tokenList
    return filteredTokenList


lexToFile :: String -> String -> IO ()
lexToFile fileName stringToBeParsed = do
    case parse lexC "" stringToBeParsed of
        Left err -> putStrLn $ "Parse Failure:\n" ++ show err
        Right val -> writeFile fileName $ show val

