module Preprocessor.NewLine where
import Preprocessor.PreprocessingParser
    ( PreprocessingParser
    , stringParserSatisfy
    )
import Lexer.Lexer
    (newLineAsString)
import Text.Parsec.Prim
    ((<?>))

newLine :: PreprocessingParser
newLine = stringParserSatisfy newLineAsString <?> "Newline"
