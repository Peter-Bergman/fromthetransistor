module NewLine where
import PreprocessingParser
import Lexer.Lexer (newLineAsString)
import Text.Parsec.String

newLine :: PreprocessingParser
newLine = stringParserSatisfy newLineAsString

