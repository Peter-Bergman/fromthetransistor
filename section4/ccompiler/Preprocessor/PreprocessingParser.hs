module PreprocessingParser where
import Data.Map
import Lexer.Lexer
    ( lexString
    , horizontalSpacing
    )
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


stringSatisfyTNoPrecedingWhiteSpace :: (String -> Bool) -> (String -> t) -> PreprocessingParserX t
stringSatisfyTNoPrecedingWhiteSpace stringCheck stringToT = tokenPrim show nextPosition maybeList
    where
        nextPosition position x xs = updatePosString position x
        maybeList x = if (stringCheck x) then Just $ stringToT x else Nothing

horizontalWhiteSpace :: PreprocessingParserX Integer
horizontalWhiteSpace = stringSatisfyTNoPrecedingWhiteSpace (stringParserToStringChecker horizontalSpacing) $ toInteger . length
-- On the line above, I could (and probably will change tabs to be 4 white space characters.
-- That would require me to use a different function than toInteger . length

anyStringToken :: PreprocessingParserX String
anyStringToken = stringSatisfyT stringToTrue id
    where
        stringToTrue :: String -> Bool
        stringToTrue x = True

stringSatisfy :: (String -> Bool) -> PreprocessingParser
stringSatisfy stringCheck = stringSatisfyT stringCheck singleton
    where
    singleton x = [x]

stringSatisfyT :: (String -> Bool) -> (String -> t) -> PreprocessingParserX t
stringSatisfyT stringCheck stringToT = skipMany horizontalWhiteSpace >> stringSatisfyTNoPrecedingWhiteSpace stringCheck stringToT

stringSatisfy_ :: (String -> Bool) -> PreprocessingParserX ()
stringSatisfy_ stringCheck = stringSatisfyT stringCheck $ \_ -> ()

-- Note that the resulting PreprocessingParser will always fail if the input
-- parser skips any characters.
stringParserSatisfy :: Parser String -> PreprocessingParser
stringParserSatisfy parser = stringSatisfy (stringParserToStringChecker parser)

-- Abstracted version of stringParserSatisfy that has return type PreprocessingParserX t
stringParserSatisfyT :: Parser String -> (String -> t) -> PreprocessingParserX t
stringParserSatisfyT parser stringToT = stringSatisfyT (stringParserToStringChecker parser) stringToT

stringParserSatisfy_ :: Parser String -> PreprocessingParserX ()
stringParserSatisfy_ stringParser = stringParserSatisfyT stringParser $ \_ -> ()

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

lexThenParse :: Show t => PreprocessingParserX t -> String -> IO ()
lexThenParse parser inputString =
    testParse parser lexedTokens
    where
        lexedTokens :: [String]
        lexedTokens = lexString inputString

tryMaybe :: PreprocessingParserX t -> PreprocessingParserX (Maybe t)
tryMaybe inputParser = try (optionMaybe inputParser) <|> return Nothing

