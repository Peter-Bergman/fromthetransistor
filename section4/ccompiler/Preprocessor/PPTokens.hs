module PPTokens 
    ( ppToken
    , ppTokens
    ) where
import AbstractSyntaxTree
    ( PPToken
    , PPTokens
    )
import Data.List.NonEmpty
    (fromList)
import Lexer.PreprocessingToken as LPPT
import PreprocessingParser
    ( PreprocessingParserX
    , stringParserSatisfyT
    )
import Text.Parsec
import Text.Parsec.Combinator

ppToken :: PreprocessingParserX PPToken
ppToken = stringParserSatisfyT LPPT.preprocessingToken id <?> "PPToken"

ppTokens :: PreprocessingParserX PPTokens
ppTokens = do
    tokens <- many1 ppToken <?> "PPTokens"
    return $ fromList tokens

