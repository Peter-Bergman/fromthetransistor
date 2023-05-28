module Preprocessor.EndIfLine where
import CustomCombinators
    (nullifyParser)
import Preprocessor.NewLine
    (newLine)
import Preprocessor.Octothorpe
    (octothorpe)
import Preprocessor.PreprocessingParser
    ( PreprocessingParserX
    , stringSatisfy_
    )
import Text.Parsec
    ( try
    , (<?>)
    )

endIfLine :: PreprocessingParserX ()
endIfLine = nullifyParser ( try (octothorpe >> endIf >> newLine) <?> "EndIfLine" )

endIf :: PreprocessingParserX ()
endIf = stringSatisfy_ (=="endif") <?> "endif"
