module Preprocessor.NullDirective (nullDirective) where
import AbstractSyntaxTree
    ( ControlLine
        (NullDirective)
    )
import Preprocessor.NewLine
    (newLine)
import Preprocessor.Octothorpe
    (octothorpe)
import Preprocessor.PreprocessingParser
    (PreprocessingParserX)

nullDirective :: PreprocessingParserX ControlLine
nullDirective = octothorpe >> newLine >> return NullDirective

