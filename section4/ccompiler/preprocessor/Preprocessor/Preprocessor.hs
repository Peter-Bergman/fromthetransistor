module Preprocessor where
import Data.List
import Data.Maybe
import Data.Text
import Lexer
import ParserStringTransformer (stringParserToStringChecker)
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.Parsec.String


--type StringListParser = GenParser String () [String]

-- This is just a shorthand for stringParserToStringChecker
parserToChecker :: Parser String -> (String -> Bool)
parserToChecker = stringParserToStringChecker


--preprocessingFile :: Parser String
--preprocessingFile = do
--    option [] (try group)

--group :: StringListParser
--group = do
    --groupParts <- many1 groupPart
    --let groupPartsFlattened = intercalate "" groupParts


--ppTokens :: StringListParser
--ppTokens = do
    

-- probably want to change nextPosition function definition in the future
-- especially if I change the output of the lexer to be list of 
-- (position, string) pair
stringToken :: GenParser String () [String]
stringToken = stringSatisfy stringToTrue
    where
        stringToTrue :: String -> Bool
        stringToTrue x = True

stringSatisfy :: (String -> Bool) -> GenParser String () [String]
stringSatisfy stringCheck = tokenPrim show nextPosition maybeList
    where
        nextPosition position x xs = updatePosString position x
        maybeList x = if (stringCheck x) then Just [x] else Nothing

stringParserSatisfy :: (Parser String) -> GenParser String () [String]
stringParserSatisfy parser = stringSatisfy (parserToChecker parser)


