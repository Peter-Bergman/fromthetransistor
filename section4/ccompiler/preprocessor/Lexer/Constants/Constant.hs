module Main where
import System.Environment
import Text.Parsec.Char
import Text.ParserCombinators.Parsec


main :: IO()
main = do
    args <- getArgs
    let firstArg = args !! 0
    putStrLn (readExpr (firstArg))

mainParser = constantParser

readExpr :: String -> String
readExpr input = case parse (mainParser) "constantParser" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val


data Constant = 
    IntegerConstant |
    FloatingConstant |
    EnumerationConstant |
    CharacterConstant

constantParser :: Parser Constant
constantParser = do
    constant <-
    	try (integerParser <|>
    	try (FloatingParser) <|>
    	try (EnumerationParser) <|>
    	CharacterConstant
    return constant


integerConstant :: Parser 
