module PPTokens where 
import Data.List
import Lexer.PreprocessingToken as LPPT
import PreprocessingParser
import Text.Parsec
import Text.Parsec.Combinator

ppToken :: PreprocessingParser
ppToken = stringParserSatisfy LPPT.preprocessingToken

ppTokens :: PreprocessingParser
ppTokens = do
    tokens <- many1 ppToken
    return $ foldl (++) [] tokens

