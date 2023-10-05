module Lexer.Lexer where
import AbstractSyntaxTree
import CustomCombinators
import Data.Char
import Lexer.PreprocessingToken
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String
import System.Environment


data PreprocessorLexeme =
    PreprocessingTokenLexeme (String, PreprocessingToken) |
    StringLexeme String

instance Show PreprocessorLexeme where
    show preprocessorLexeme = case preprocessorLexeme of
        (PreprocessingTokenLexeme stringAndPreprocessingToken') -> fst stringAndPreprocessingToken'
        (StringLexeme string') -> string'

backSlashedNewLines :: Parser Char
backSlashedNewLines = try (string"\\\n") >> return ' '

horizontalSpacing :: Parser String
horizontalSpacing = tryWithFailMessage "Horzontal Spacing" $ many1 (horizontalSpace <|> backSlashedNewLines)

horizontalSpacingStringLexeme :: Parser PreprocessorLexeme
horizontalSpacingStringLexeme = try $ do
    parsedHorizontalSpacingString <- horizontalSpacing
    return $ StringLexeme parsedHorizontalSpacingString

horizontalSpace :: Parser Char
horizontalSpace = satisfy (isHorizontalSpace)

isHorizontalSpace :: Char -> Bool
isHorizontalSpace character = isSpace character && ('\n' /= character)

newLineAsString :: Parser String
newLineAsString = do
    parsedNewLineCharacter <- char '\n'
    return [ parsedNewLineCharacter ]

newLineStringLexeme :: Parser PreprocessorLexeme
newLineStringLexeme = simpleExpression newLineAsString StringLexeme

lexErrorMessage :: Show t => t -> String
lexErrorMessage error_ = "Lex error at " ++ show error_

preprocessingTokenLexeme :: Parser PreprocessorLexeme
preprocessingTokenLexeme = simpleExpression (parseADTAndConsumedInput preprocessingToken) PreprocessingTokenLexeme

lexParser :: Parser [String]
lexParser = try $ do
    parsedLexemes <- many $ horizontalSpacingStringLexeme <|> preprocessingTokenLexeme <|> newLineStringLexeme
    let lexemesAsStrings = map show parsedLexemes
    return lexemesAsStrings

lexString :: String -> [String]
lexString inputString = case (runParser lexParser () "" inputString) of
    Left err -> error $ lexErrorMessage err
    Right value -> value

lexToFile :: String -> String -> IO ()
lexToFile fileName stringToBeParsed = do
    case parse lexParser "" stringToBeParsed of
        Left err -> putStrLn $ lexErrorMessage err
        Right val -> writeFile fileName $ show val

