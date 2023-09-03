module Preprocessor.Identifier (identifier) where
import AbstractSyntaxTree
    (Identifier)
import qualified CharTokenParsers.Identifiers.Identifier
    (identifier)
import Preprocessor.PreprocessingParser
    ( PreprocessingParserX
    , charToStringTokenParserSkipPrecedingSpaces
    )
import Text.Parsec.Prim
    ((<?>))

identifier :: PreprocessingParserX Identifier
identifier = charToStringTokenParserSkipPrecedingSpaces CharTokenParsers.Identifiers.Identifier.identifier <?> "Identifier"

