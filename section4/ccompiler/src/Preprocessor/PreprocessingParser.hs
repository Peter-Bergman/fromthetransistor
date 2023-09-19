module Preprocessor.PreprocessingParser 
( lexThenParse
, charToStringTokenParser
, charToStringTokenParserSkipPrecedingSpaces
, PreprocessingParser
, PreprocessingParserX
, stringParserToStringChecker
, stringParserSatisfy
, stringParserSatisfy_
, stringParserSatisfyT
, stringSatisfy
, stringSatisfy_
, stringSatisfyT
, stringSatisfyTNoPrecedingWhiteSpace
, testParse
) where
import CustomCombinators
    (failsIfDoesNotConsumeAllInput)
import Lexer.Lexer
    ( lexString
    , horizontalSpacing
    )
import Text.Parsec.Combinator
    (anyToken)
import Text.Parsec.Pos
    (updatePosString)
import Text.Parsec.Error
    ( errorMessages
    , Message
    , showErrorMessages
    )
import Text.Parsec.Pos
import Text.Parsec.Prim
    ( parse
    , runParser
    , tokenPrim
    , skipMany
    , parserFail
    )
import Text.Parsec.String
    ( Parser
    , GenParser
    )


type PreprocessingParser = PreprocessingParserX [String]
-- PreprocessingParserX has kind * -> *
type PreprocessingParserX = GenParser String ()


{-t1SatisfyT2NoPrecedingWhiteSpace :: (t1 -> Bool) -> (t1 -> t2) -> (SourcePos -> t1 -> [String] -> SourcePos) -> PreprocessingParserX t2
t1SatisfyT2NoPrecedingWhiteSpace t1Check t1ToT2 nextPosition = tokenPrim show nextPosition maybeList
    where
        --nextPosition position x xs = -- idk yet
        maybeList x = if (t1Check x) then Just $ t1ToT2 x else Nothing
-}

stringSatisfyTNoPrecedingWhiteSpace :: (String -> Bool) -> (String -> t) -> PreprocessingParserX t
stringSatisfyTNoPrecedingWhiteSpace checkString stringToT = tokenPrim show nextPosition maybeList
    where
        nextPosition position x xs = updatePosString position x
        maybeList x = if (checkString x) then Just $ stringToT x else Nothing

horizontalWhiteSpace :: PreprocessingParserX Integer
horizontalWhiteSpace = stringSatisfyTNoPrecedingWhiteSpace (stringParserToStringChecker horizontalSpacing) $ toInteger . length
-- TODO: On the line above, I could (and probably will change tabs to be 4 white space characters.
-- NOTE: That would require me to use a different function than toInteger . length

skipAnyHorizontalWhiteSpace :: PreprocessingParserX ()
skipAnyHorizontalWhiteSpace = skipMany horizontalWhiteSpace

stringSatisfy :: (String -> Bool) -> PreprocessingParser
stringSatisfy checkString = stringSatisfyT checkString singleton
    where
        singleton x = [x]

stringSatisfyT :: (String -> Bool) -> (String -> t) -> PreprocessingParserX t
stringSatisfyT checkString stringToT = skipAnyHorizontalWhiteSpace >> stringSatisfyTNoPrecedingWhiteSpace checkString stringToT

stringSatisfy_ :: (String -> Bool) -> PreprocessingParserX ()
stringSatisfy_ checkString = stringSatisfyT checkString $ \_ -> ()

-- NOTE: the resulting PreprocessingParser will always fail if the input
-- parser skips any characters.
stringParserSatisfy :: Parser String -> PreprocessingParser
stringParserSatisfy parser = stringSatisfy (stringParserToStringChecker parser)

-- TODO: Can I fully replace all references of stringParserSatisfyT et al. with charToStringTokenParser?
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

charToStringTokenParserSkipPrecedingSpaces :: Parser t -> PreprocessingParserX t
charToStringTokenParserSkipPrecedingSpaces parser = skipAnyHorizontalWhiteSpace >> charToStringTokenParser parser


charToStringTokenParser :: Parser t -> PreprocessingParserX t
charToStringTokenParser parser = do
    parsedToken <- anyToken
    let parsedResult = parseStringWithCharTokenParser parsedToken
    case (parsedResult) of
        Left err -> parserFail $ showParseErrorNoPos err
        Right parsedT -> return parsedT
    where
        parseStringWithCharTokenParser tokenToParse = parse (failsIfDoesNotConsumeAllInput parser) "" tokenToParse
        showErrorMessagesCustom :: [Message] -> String
        showErrorMessagesCustom messages = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of string token" messages
        showParseErrorNoPos parseError = showErrorMessagesCustom $ errorMessages parseError

testParse :: Show t => PreprocessingParserX t -> [String] -> IO ()
testParse parser tokenList = case (runParser parser () "" tokenList) of
    Left err -> putStr "parse error at " >> print err
    Right value -> print value

lexThenParse :: Show t => PreprocessingParserX t -> String -> IO ()
lexThenParse parser inputString =
    testParse parser lexedTokens
    where
        lexedTokens :: [String]
        lexedTokens = lexString inputString

