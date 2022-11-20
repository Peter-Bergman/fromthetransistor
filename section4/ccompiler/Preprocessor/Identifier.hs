module Identifier (identifier) where
import qualified CharTokenParsers.Identifiers.Identifier
    (identifier)
import PreprocessingParser
    ( PreprocessingParserX
    , stringParserSatisfyT
    )
import Text.Parsec.Prim
    ((<?>))

identifier :: PreprocessingParserX String
identifier = stringParserSatisfyT CharTokenParsers.Identifiers.Identifier.identifier id <?> "Identifier"

