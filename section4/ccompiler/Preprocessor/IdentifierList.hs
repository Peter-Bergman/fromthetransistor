module IdentifierList (identifierList) where
import CharTokenParsers.Identifiers.Identifier
    (identifier)
import Data.List
    (intercalate)
import PreprocessingParser
    ( PreprocessingParser
    , stringParserSatisfy
    , stringSatisfy
    )
import Text.Parsec
    (many)
import Text.Parsec.Prim
    (try)

identifierList :: PreprocessingParser
identifierList = do
    firstIdentifier <- identifierPreprocessingParser
    restOfIdentifiersUnflattened <- many $ try anotherIdentifier

    let identifierListRaw = firstIdentifier : restOfIdentifiersUnflattened
    let flattenedCommaSeparatedIdentifierList = intercalate [","] identifierListRaw

    return flattenedCommaSeparatedIdentifierList


identifierPreprocessingParser :: PreprocessingParser
identifierPreprocessingParser = stringParserSatisfy identifier

anotherIdentifier :: PreprocessingParser
anotherIdentifier = do
    comma
    parsedIdentifier <- identifierPreprocessingParser
    return $ parsedIdentifier

comma :: PreprocessingParser
comma = stringSatisfy (==",")

