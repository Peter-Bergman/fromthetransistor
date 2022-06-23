module Main where
import System.Environment
import Text.ParserCombinators.Parsec

main :: IO()
main = do
    args <- getArgs
    let firstArg = args !! 0
    putStrLn (readExpr (firstArg))

mainParser = caseOrCharParser

readExpr :: String -> String
readExpr input = case parse (mainParser) "pointers" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val


{-caseOrCharParser = do
    keyword <-
        try (string "case")
        <|> try (string "char")
        <|> try (string "continue")
    return keyword -}
caseOrCharParser :: Parser Keyword
caseOrCharParser = do
    try (caseParser)
    <|> charParser

caseParser :: Parser Keyword
caseParser = do
    string "case"
    return Case

charParser :: Parser Keyword
charParser = do
    string "char"
    return Char

data Keyword = 
    Auto |
    Break |
    Case |
    Char


instance Show Keyword where
    show (Auto) = "Keyword \"auto\""
    show (Break) = "Keyword \"break\""
    show (Case) = "Keyword \"case\""
    show (Char) = "Keyword \"char\""
    show (


{-keywordParser :: Parser Keyword
keywordParser = do
    keywordString <- do {
        try (string "auto") <|>
        try (string "break") <|>
        try (string "case")
        }
    let keywordToReturn = case keywordString of
    	"auto" -> Auto
    	"break" -> Break
    	"case" -> Case
    return keywordToReturn $ keywordString-}
