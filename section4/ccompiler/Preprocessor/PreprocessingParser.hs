module PreprocessingParser where
import Data.Map
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.Parsec.String
import Data.List


type Dictionary = Map String String

type PreprocessingParser = PreprocessingParserX [String]
-- PreprocessingParserX has kind * -> *
type PreprocessingParserX = GenParser String Dictionary


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

stringSatisfyT :: (String -> Bool) -> (String -> t) -> PreprocessingParserX t
stringSatisfyT stringCheck stringToT = tokenPrim show nextPosition maybeList
    where
        nextPosition position x xs = updatePosString position x
        maybeList x = if (stringCheck x) then Just $ stringToT x else Nothing

-- Note that the resulting PreprocessingParser will always fail if the input
-- parser skips any characters.
stringParserSatisfy :: Parser String -> PreprocessingParser
stringParserSatisfy parser = stringSatisfy (stringParserToStringChecker parser)

-- Abstracted version of stringParserSatisfy that has return type PreprocessingParserX t
stringParserSatisfyT :: Parser String -> (String -> t) -> PreprocessingParserX t
stringParserSatisfyT parser stringToT = stringSatisfyT (stringParserToStringChecker parser) stringToT

stringParserToStringChecker :: Parser String -> (String -> Bool)
stringParserToStringChecker parser inputString = 
    case (parsedString) of
        Left err -> False
        Right xs -> xs == inputString
    where
        parsedString = parse parser "" inputString

testParse :: Show t => PreprocessingParserX t -> [String] -> IO ()
testParse parser tokenList = case (runParser parser empty "" tokenList) of
    Left err -> putStr "parse error at " >> print err
    Right value -> print value

tryMaybe :: PreprocessingParserX t -> PreprocessingParserX (Maybe t)
tryMaybe inputParser = try (optionMaybe inputParser) <|> return Nothing

