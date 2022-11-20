module Comma (comma) where
import PreprocessingParser
    ( PreprocessingParserX
    , stringSatisfy_
    )

comma :: PreprocessingParserX ()
comma = stringSatisfy_ (==",")

