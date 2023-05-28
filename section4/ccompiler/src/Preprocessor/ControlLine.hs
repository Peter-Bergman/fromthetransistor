module Preprocessor.ControlLine (controlLine) where
import AbstractSyntaxTree
    (ControlLine)
import Preprocessor.DefineDirective
    (defineDirective)
import Preprocessor.ErrorDirective
    (errorDirective)
import Preprocessor.IncludeDirective
    (includeDirective)
import Preprocessor.LineDirective
    (lineDirective)
import Preprocessor.NullDirective
    (nullDirective)
import Preprocessor.PragmaDirective
    (pragmaDirective)
import Preprocessor.PreprocessingParser
    (PreprocessingParserX)
import Preprocessor.UndefDirective
    (undefDirective)
import Text.Parsec.Prim
    ( try
    , (<|>)
    , (<?>)
    )


controlLine :: PreprocessingParserX ControlLine
controlLine = 
    try (includeDirective) <|>
    try (defineDirective) <|>
    try (undefDirective) <|>
    try (lineDirective) <|>
    try (errorDirective) <|>
    try (pragmaDirective) <|>
    try (nullDirective)

