module NonDirective (nonDirective) where
import AbstractSyntaxTree
    ( GroupPart
        (NonDirective)
    )
import NewLine
    (newLine)
import PPTokens
    (ppTokens)
import PreprocessingParser
    (PreprocessingParserX)

nonDirective :: PreprocessingParserX GroupPart
nonDirective = do
    parsedTokens <- ppTokens
    newLine
    return $ NonDirective parsedTokens

