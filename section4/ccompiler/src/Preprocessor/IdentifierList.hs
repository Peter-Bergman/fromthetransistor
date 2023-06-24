module Preprocessor.IdentifierList (identifierList) where
import AbstractSyntaxTree
    ( IdentifierList
        (IdentifierList)
    )
import Preprocessor.Comma
    (comma)
import CustomCombinators
    (sepBy1NonConsumption)
import Preprocessor.Identifier
    (identifier)
import Data.List.NonEmpty
    (fromList)
import Preprocessor.PreprocessingParser
    (PreprocessingParserX)

identifierList :: PreprocessingParserX IdentifierList
identifierList = do
    listOfIdentifiers <- sepBy1NonConsumption identifier comma
    -- Convert the list of identifiers to a non empty list
    return $ IdentifierList $ fromList listOfIdentifiers

