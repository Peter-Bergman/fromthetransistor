module Preprocessor.LParen (lParen) where
import Preprocessor.PreprocessingParser
    ( PreprocessingParserX
    , stringSatisfyTNoPrecedingWhiteSpace
    )
import Text.Parsec.Prim
    ((<?>))

lParen :: PreprocessingParserX String
lParen = stringSatisfyTNoPrecedingWhiteSpace (=="(") id <?> "Left Parenthesis"
