module Preprocessor.PPTokens 
    ( ppToken
    , ppTokens
    ) where
import AbstractSyntaxTree
    ( PreprocessingToken
    , PPTokens
        (PPTokens)
    )
import CustomCombinators
    (many1NonEmpty)
import Lexer.PreprocessingToken as LPPT
import Preprocessor.PreprocessingParser
    ( PreprocessingParserX
    , stringParserSatisfyT
    , charToStringTokenParser
    )
import Text.Parsec
import Text.Parsec.Combinator

ppToken :: PreprocessingParserX PreprocessingToken
ppToken = charToStringTokenParser LPPT.preprocessingToken <?> "Preprocessing Token"

ppTokens :: PreprocessingParserX PPTokens
ppTokens = do
    parsedTokens <- many1NonEmpty (try ppToken) <?> "PPTokens"
    let ppTokensToReturn = PPTokens parsedTokens
    return ppTokensToReturn

