module ParserStringTransformer where
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String
import Data.List

word :: Parser String
word = many1 letter

--wordList :: GenParser String () [String]
--wordList = do
    --let 
    --
wordChecker :: String -> Bool
wordChecker = stringParserToStringChecker word


stringParserToStringChecker :: Parser String -> (String -> Bool)
stringParserToStringChecker parser inputString = 
    case (parsedString) of
        Left err -> False
        Right xs -> xs == inputString
    where
        parsedString = parse parser "" inputString


