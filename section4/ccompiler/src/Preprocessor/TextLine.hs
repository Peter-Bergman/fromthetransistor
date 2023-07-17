module Preprocessor.TextLine (textLine) where
import AbstractSyntaxTree
    ( GroupPart
        (TextLine)
    )
import Preprocessor.NewLine
    (newLine)
import Preprocessor.PreprocessingParser
    (PreprocessingParserX)
import Preprocessor.PPTokens
    (ppTokens)
import Text.Parsec
    (optionMaybe)

textLine :: PreprocessingParserX GroupPart
textLine = do
    parsedTokens <- optionMaybe ppTokens
    newLine
    return $ TextLine parsedTokens