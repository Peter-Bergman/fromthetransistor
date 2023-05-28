module Preprocessor.PPTokens 
    ( ppToken
    , ppTokens
    ) where
import AbstractSyntaxTree
    ( PreprocessingToken
    , PPTokens
    )
import Data.List.NonEmpty
    (fromList)
import Lexer.PreprocessingToken as LPPT
import Preprocessor.PreprocessingParser
    ( PreprocessingParserX
    , stringParserSatisfyT
    )
import Text.Parsec
import Text.Parsec.Combinator

ppToken :: PreprocessingParserX PreprocessingToken
ppToken = stringParserSatisfyT LPPT.preprocessingToken id <?> "Preprocessing Token"

ppTokens :: PreprocessingParserX PPTokens
ppTokens = do
    tokens <- many1 (try ppToken) <?> "PPTokens"
    return $ fromList tokens

