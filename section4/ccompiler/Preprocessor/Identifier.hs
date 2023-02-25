module Identifier (identifier) where
import AbstractSyntaxTree
    (Identifier)
import qualified CharTokenParsers.Identifiers.Identifier
    (identifier)
import PreprocessingParser
    ( PreprocessingParserX
    , stringParserSatisfyT
    )
import Text.Parsec.Prim
    ((<?>))

identifier :: PreprocessingParserX Identifier
identifier = stringParserSatisfyT CharTokenParsers.Identifiers.Identifier.identifier id <?> "Identifier"

