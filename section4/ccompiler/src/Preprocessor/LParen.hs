module Preprocessor.LParen (lParen) where
import PreprocessingParser
    ( PreprocessingParserX
    , stringSatisfyTNoPrecedingWhiteSpace
    )
import Text.Parsec.Prim
    ((<?>))

lParen :: PreprocessingParserX String
lParen = stringSatisfyTNoPrecedingWhiteSpace (=="(") id <?> "Left Parenthesis"