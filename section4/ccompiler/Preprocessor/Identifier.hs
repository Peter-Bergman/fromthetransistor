module Identifier (identifier) where
import qualified CharTokenParsers.Identifiers.Identifier
    (identifier)
import PreprocessingParser
    ( PreprocessingParserX
    , stringParserSatisfyT
    )

identifier :: PreprocessingParserX String
identifier = stringParserSatisfyT CharTokenParsers.Identifiers.Identifier.identifier id

