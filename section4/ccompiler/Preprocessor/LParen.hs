module LParen (lParen) where
import Data.Char
    (isSpace)
import PreprocessingParser
    ( PreprocessingParser
    , stringSatisfy
    )
import Text.Parsec.Prim
    ( (<?>)
    , parserFail
    )

lParen :: String -> PreprocessingParser
lParen precedingString = 
    let lastCharacter = last precedingString in
    if (isSpace lastCharacter) then parserFail "Unexpected spacing before expected left parenthesis"
        else (stringSatisfy (=="(")) <?> "Left Parenthesis"

