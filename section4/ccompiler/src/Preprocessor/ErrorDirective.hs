module Preprocessor.ErrorDirective (errorDirective) where
import AbstractSyntaxTree
    ( ControlLine
        (ErrorDirective)
    )
import CustomCombinators
    (tryMaybe)
import Preprocessor.NewLine
    (newLine)
import Preprocessor.Octothorpe
    (octothorpe)
import Preprocessor.PPTokens
    (ppTokens)
import Preprocessor.PreprocessingParser
    ( PreprocessingParserX
    , stringSatisfy_
    )


errorDirective:: PreprocessingParserX ControlLine
errorDirective = do
    errorPrefix
    parsedPPTokens <- tryMaybe ppTokens
    newLine
    return $ ErrorDirective parsedPPTokens

errorPrefix :: PreprocessingParserX ()
errorPrefix = octothorpe >> error_

error_ :: PreprocessingParserX ()
error_ = stringSatisfy_ (=="error")
