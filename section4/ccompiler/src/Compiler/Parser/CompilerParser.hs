module Compiler.Parser.CompilerParser where
import Text.Parsec.String
    (GenParser)

type CompilerParser = CompilerParserX [String]

type CompilerParserX = GenParser String ()


