module Preprocessor.PragmaDirective (pragmaDirective) where
import AbstractSyntaxTree
    ( ControlLine
        (PragmaDirective)
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

pragmaDirective :: PreprocessingParserX ControlLine
pragmaDirective = do
    pragmaPrefix
    parsedPPTokens <- tryMaybe ppTokens
    _ <- newLine
    return $ PragmaDirective parsedPPTokens

pragmaPrefix :: PreprocessingParserX ()
pragmaPrefix = octothorpe >> pragma

pragma :: PreprocessingParserX ()
pragma = stringSatisfy_ (=="pragma")

