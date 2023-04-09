module Preprocessor.ReplacementList (replacementList) where
import AbstractSyntaxTree
    (ReplacementList)
import CustomCombinators
    (tryMaybe)
import PreprocessingParser
    (PreprocessingParserX)
import PPTokens
    (ppTokens)

replacementList :: PreprocessingParserX ReplacementList
replacementList = tryMaybe ppTokens