module Preprocessor.ReplacementList (replacementList) where
import AbstractSyntaxTree
    ( ReplacementList
        (ReplacementList)
    )
import CustomCombinators
    ( simpleExpression
    , tryMaybe
    )
import Preprocessor.PreprocessingParser
    (PreprocessingParserX)
import Preprocessor.PPTokens
    (ppTokens)

replacementList :: PreprocessingParserX ReplacementList
replacementList = simpleExpression (tryMaybe ppTokens) ReplacementList

