module LineDirective (lineDirective) where
import AbstractSyntaxTree
    ( ControlLine( LineDirective ) )
import Octothorpe
    (octothorpe)
import NewLine
    (newLine)
import PPTokens
    (ppTokens)
import PreprocessingParser
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

