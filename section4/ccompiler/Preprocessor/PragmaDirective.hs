module PragmaDirective (pragmaDirective) where
import AbstractSyntaxTree
    ( ControlLine
        (PragmaDirective)
    )
import CustomCombinators
    (tryMaybe)
import NewLine
    (newLine)
import Octothorpe
    (octothorpe)
import PPTokens
    (ppTokens)
import PreprocessingParser
    ( PreprocessingParserX
    , stringSatisfy_
    )

pragmaDirective :: PreprocessingParserX ControlLine
pragmaDirective = do
    pragmaPrefix
    parsedPPTokens <- tryMaybe ppTokens
    newLine
    return $ PragmaDirective parsedPPTokens

pragmaPrefix :: PreprocessingParserX ()
pragmaPrefix = octothorpe >> pragma

pragma :: PreprocessingParserX ()
pragma = stringSatisfy_ (=="pragma")

