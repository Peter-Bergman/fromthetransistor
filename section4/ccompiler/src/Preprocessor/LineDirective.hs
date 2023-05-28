module Preprocessor.LineDirective (lineDirective) where
import AbstractSyntaxTree
    ( ControlLine
        (LineDirective)
    )
import Preprocessor.Octothorpe
    (octothorpe)
import Preprocessor.NewLine
    (newLine)
import Preprocessor.PPTokens
    (ppTokens)
import Preprocessor.PreprocessingParser
    ( PreprocessingParserX
    , stringSatisfy_
    )

lineDirective :: PreprocessingParserX ControlLine
lineDirective = do
    linePrefix
    parsedPPTokens <- ppTokens
    newLine
    return $ LineDirective parsedPPTokens

linePrefix :: PreprocessingParserX ()
linePrefix = octothorpe >> line

line :: PreprocessingParserX ()
line = stringSatisfy_ (=="line")
