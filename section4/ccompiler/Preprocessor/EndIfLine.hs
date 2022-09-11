module EndIfLine where
import NewLine
import Octothorpe
import PreprocessingParser
import Text.Parsec

endIfLine :: PreprocessingParser
endIfLine = do
    parsedOctothorpe <- octothorpe
    parsedEndIf <- endIf
    parsedNewLine <- newLine
    return $ parsedOctothorpe ++ parsedEndIf ++ parsedNewLine

endIf :: PreprocessingParser
endIf = stringParserSatisfy (string "endif")

