module ControlLine (controlLine) where
import AbstractSyntaxTree
    (ControlLine)
import DefineDirective
    (defineDirective)
import ErrorDirective
    (errorDirective)
import IncludeDirective
    (includeDirective)
import LineDirective
    (lineDirective)
import NullDirective
    (nullDirective)
import PragmaDirective
    (pragmaDirective)
import PreprocessingParser
    (PreprocessingParserX)
import UndefDirective
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

