module Lexer.Lexer where
import Data.Char
import Data.List
import Lexer.PreprocessingToken
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String
import System.Environment


backSlashedNewLines :: Parser Char
backSlashedNewLines = try (string"\\\n") >> return ' '

horizontalSpacing :: Parser String
horizontalSpacing = many1 (horizontalSpace <|> backSlashedNewLines) <?> "Horzontal Spacing"

horizontalSpace :: Parser Char
horizontalSpace = satisfy (isHorizontalSpace)

isHorizontalSpace :: Char -> Bool
isHorizontalSpace character = isSpace character && ('\n' /= character)

newLineAsString :: Parser String
newLineAsString = do
    parsedNewLineCharacter <- char '\n'
    return [ parsedNewLineCharacter ]

lexErrorMessage :: Show t => t -> String
lexErrorMessage error_ = "Lex error at " ++ show error_

lexParser :: Parser [String]
lexParser = many $ horizontalSpacing <|> preprocessingToken <|> newLineAsString

lexString :: String -> [String]
lexString inputString = case (runParser lexParser () "" inputString) of
            Left err -> error $ lexErrorMessage err
            Right value -> value

lexToFile :: String -> String -> IO ()
lexToFile fileName stringToBeParsed = do
    case parse lexParser "" stringToBeParsed of
        Left err -> putStrLn $ lexErrorMessage err
        Right val -> writeFile fileName $ show val

