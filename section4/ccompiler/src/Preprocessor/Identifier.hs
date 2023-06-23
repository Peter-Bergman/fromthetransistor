module Preprocessor.Identifier (identifier) where
import AbstractSyntaxTree
    (Identifier)
import qualified CharTokenParsers.Identifiers.Identifier
    (identifier)
import Preprocessor.PreprocessingParser
    ( PreprocessingParserX
    , charToStringTokenParser
    )
import Text.Parsec.Prim
    ((<?>))

identifier :: PreprocessingParserX Identifier
identifier = charToStringTokenParser CharTokenParsers.Identifiers.Identifier.identifier <?> "Identifier"

