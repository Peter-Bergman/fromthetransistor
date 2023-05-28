module Preprocessor.Comma (comma) where
import Preprocessor.PreprocessingParser
    ( PreprocessingParserX
    , stringSatisfy_
    )

comma :: PreprocessingParserX ()
comma = stringSatisfy_ (==",")
