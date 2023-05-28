module Preprocessor.IncludeDirective (includeDirective) where
import AbstractSyntaxTree
    ( ControlLine
        (IncludeDirective)
    )
import Preprocessor.NewLine
    (newLine)
import Preprocessor.Octothorpe
    (octothorpe)
import Preprocessor.PPTokens
    (ppTokens)
import Preprocessor.PreprocessingParser
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
