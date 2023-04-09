module Preprocessor.NonDirectiveLine
( nonDirective
, nonDirectiveLine
) where
import AbstractSyntaxTree
    ( GroupPart
        (NonDirective)
    )
import NewLine
    (newLine)
import Octothorpe
    (octothorpe)
import PPTokens
    (ppTokens)
import PreprocessingParser
    (PreprocessingParserX)

nonDirective :: PreprocessingParserX GroupPart
nonDirective = do
    parsedTokens <- ppTokens
    newLine
    return $ NonDirective parsedTokens

nonDirectiveLine :: PreprocessingParserX GroupPart
nonDirectiveLine = octothorpe >> nonDirective
