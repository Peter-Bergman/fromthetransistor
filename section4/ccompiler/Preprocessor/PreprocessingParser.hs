module PreprocessingParser where
import Data.Map
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.Parsec.String
import Data.List


type Dictionary = Map String String

type PreprocessingParser = GenParser String Dictionary [String]


anyStringToken :: PreprocessingParser
anyStringToken = stringSatisfy stringToTrue
    where
        stringToTrue :: String -> Bool
        stringToTrue x = True

stringSatisfy :: (String -> Bool) -> PreprocessingParser
stringSatisfy stringCheck = tokenPrim show nextPosition maybeList
    where
        nextPosition position x xs = updatePosString position x
        maybeList x = if (stringCheck x) then Just [x] else Nothing

-- Note that the resulting PreprocessingParser will always fail if the input
-- parser skips any characters.
stringParserSatisfy :: (Parser String) -> PreprocessingParser
stringParserSatisfy parser = stringSatisfy (stringParserToStringChecker parser)

stringParserToStringChecker :: Parser String -> (String -> Bool)
stringParserToStringChecker parser inputString = 
    case (parsedString) of
        Left err -> False
        Right xs -> xs == inputString
    where
        parsedString = parse parser "" inputString

testParse :: PreprocessingParser -> [String] -> IO ()
testParse parser tokenList = case (runParser parser empty "" tokenList) of
    Left err -> putStr "parse error at " >> print err
    Right value -> print value

