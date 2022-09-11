module NonDirective where
import NewLine
import PPTokens
import PreprocessingParser

nonDirective :: PreprocessingParser
nonDirective = do
    parsedTokens <- ppTokens
    parsedNewLine <- newLine
    return $ parsedTokens ++ parsedNewLine

