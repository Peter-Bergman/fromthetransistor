module ReplacementList where
import PreprocessingParser
import PPTokens
import Text.Parsec.Combinator

replacementList :: PreprocessingParser
replacementList = option [] ppTokens

