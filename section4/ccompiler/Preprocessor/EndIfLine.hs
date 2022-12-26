module EndIfLine where
import CustomCombinators
    (nullifyParser)
import NewLine
    (newLine)
import Octothorpe
    (octothorpe)
import PreprocessingParser
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

