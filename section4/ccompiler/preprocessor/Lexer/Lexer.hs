module Lexer where
import Data.List
import PreprocessingToken
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
    

lexC :: Parser [String]
lexC = do
    tokenList <- many $ spacingCompressionLexer <|> preprocessingToken
    return tokenList


lexToFile :: String -> String -> IO ()
lexToFile fileName stringToBeParsed = do
    case parse lexC "" stringToBeParsed of
        Left err -> putStrLn $ "Parse Failure:\n" ++ show err
        Right val -> writeFile fileName $ show val
