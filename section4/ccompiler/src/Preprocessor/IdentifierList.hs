module Preprocessor.IdentifierList (identifierList) where
import AbstractSyntaxTree
    (IdentifierList)
import Comma
    (comma)
import CustomCombinators
    (sepBy1NonConsumption)
import Identifier
    (identifier)
import Data.List.NonEmpty
    (fromList)
import PreprocessingParser
    (PreprocessingParserX)

identifierList :: PreprocessingParserX IdentifierList
identifierList = do
    listOfIdentifiers <- sepBy1NonConsumption identifier comma
    -- Convert the list of identifiers to a non empty list
    return $ fromList listOfIdentifiers