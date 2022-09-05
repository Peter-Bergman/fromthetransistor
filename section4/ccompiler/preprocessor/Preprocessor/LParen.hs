module LParen where
import Data.Char
import PreprocessingParser
import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.String

lParen :: String -> PreprocessingParser
lParen precedingString = 
    let lastCharacter = last precedingString in
    if (isSpace lastCharacter) then fail "Unexpected spacing before parentheses"
        else stringParserSatisfy $ string "("

