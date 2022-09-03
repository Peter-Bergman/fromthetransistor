module TextLine where
import NewLine
import PreprocessingParser
import PPTokens
import Text.Parsec
import Text.Parsec.String

textLine :: PreprocessingParser
textLine = do
    parsedTokens <- ppTokens
    parsedNewLine <- newLine
    return $ parsedTokens ++ parsedNewLine
