module ReplacementList (replacementList) where
import AbstractSyntaxTree
    (ReplacementList)
import PreprocessingParser
    ( PreprocessingParserX
    , tryMaybe
    )
import PPTokens
    (ppTokens)

replacementList :: PreprocessingParserX ReplacementList
replacementList = tryMaybe ppTokens

