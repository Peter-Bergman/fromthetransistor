module Octothorpe where
import PreprocessingParser
import Text.Parsec
import Text.Parsec.String

octothorpe :: PreprocessingParser
octothorpe = stringParserSatisfy (string "#")

