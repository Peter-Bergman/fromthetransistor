module Preprocessor.NonDirectiveLine
( nonDirective
, nonDirectiveLine
) where
import AbstractSyntaxTree
    ( GroupPart
        (NonDirective)
    )
import Preprocessor.NewLine
    (newLine)
import Preprocessor.Octothorpe
    (octothorpe)
import Preprocessor.PPTokens
    (ppTokens)
import Preprocessor.PreprocessingParser
    (PreprocessingParserX)

nonDirective :: PreprocessingParserX GroupPart
nonDirective = do
    parsedTokens <- ppTokens
    newLine
    return $ NonDirective parsedTokens

nonDirectiveLine :: PreprocessingParserX GroupPart
nonDirectiveLine = octothorpe >> nonDirective
