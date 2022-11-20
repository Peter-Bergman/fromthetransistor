module ErrorDirective (errorDirective) where
import AbstractSyntaxTree
    ( ControlLine( ErrorDirective ) )
import NewLine
    (newLine)
import Octothorpe
    (octothorpe)
import PPTokens
    (ppTokens)
import PreprocessingParser
    ( PreprocessingParserX
    , stringSatisfy_
    , tryMaybe
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

