module Octothorpe (octothorpe) where
import PreprocessingParser
import Text.Parsec
import Text.Parsec.String

octothorpe :: PreprocessingParser
octothorpe = stringSatisfy (=="#") <?> "Octothorpe"

