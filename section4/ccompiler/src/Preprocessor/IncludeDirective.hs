module Preprocessor.IncludeDirective (includeDirective) where
import AbstractSyntaxTree
    ( ControlLine
        (IncludeDirective)
    )
import NewLine
    (newLine)
import Octothorpe
    (octothorpe)
import PPTokens
    (ppTokens)
import PreprocessingParser
    ( PreprocessingParser
    , PreprocessingParserX
    , stringSatisfy_
    )

includeDirective :: PreprocessingParserX ControlLine
includeDirective = do
    includePrefix
    parsedPPTokens <- ppTokens
    newLine
    return $ IncludeDirective parsedPPTokens

includePrefix :: PreprocessingParserX ()
includePrefix = octothorpe >> include

include :: PreprocessingParserX ()
include = stringSatisfy_ (=="include")