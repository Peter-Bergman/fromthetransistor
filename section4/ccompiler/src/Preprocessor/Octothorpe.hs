module Preprocessor.Octothorpe (octothorpe) where
import Preprocessor.PreprocessingParser
    (stringSatisfy)
import Text.Parsec
    ((<?>))

octothorpe :: PreprocessingParser
octothorpe = stringSatisfy (=="#") <?> "Octothorpe"
