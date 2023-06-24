module Preprocessor.StringLiteral (stringLiteral) where
import AbstractSyntaxTree
    (StringLiteral)
import qualified CharTokenParsers.StringLiterals.StringLiteral as CTP.StringLiteral
    (stringLiteral)
import Preprocessor.PreprocessingParser
    ( PreprocessingParserX
    , charToStringTokenParser
    )


stringLiteral :: PreprocessingParserX StringLiteral
stringLiteral = charToStringTokenParser CTP.StringLiteral.stringLiteral

