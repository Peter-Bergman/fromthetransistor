module NullDirective (nullDirective) where
import AbstractSyntaxTree
    ( ControlLine( NullDirective ) )
import NewLine
    (newLine)
import Octothorpe
    (octothorpe)
import PreprocessingParser
    (PreprocessingParserX)

nullDirective :: PreprocessingParserX ControlLine
nullDirective = octothorpe >> newLine >> return NullDirective

