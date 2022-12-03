module TextLine (textLine) where
import AbstractSyntaxTree
    ( GroupPart
        (TextLine)
    )
import NewLine
    (newLine)
import PreprocessingParser
    (PreprocessingParserX)
import PPTokens
    (ppTokens)
import Text.Parsec
    (optionMaybe)

textLine :: PreprocessingParserX GroupPart
textLine = do
    parsedTokens <- optionMaybe ppTokens
    newLine
    return $ TextLine parsedTokens

