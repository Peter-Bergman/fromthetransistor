module Preprocessor.UndefDirective where
import AbstractSyntaxTree
    ( ControlLine
        (UndefDirective)
    )
import CustomCombinators
    (nullifyParser)
import Identifier
    (identifier)
import NewLine
    (newLine)
import Octothorpe
    (octothorpe)
import PreprocessingParser
    ( PreprocessingParserX
    , stringSatisfy_
    )
import Text.Parsec.Combinator
    (between)
import Text.Parsec.Prim
    ((<?>))


undefDirective :: PreprocessingParserX ControlLine
undefDirective = do
    parsedIdentifier <- between undefPrefix newLine identifier
    return $ UndefDirective parsedIdentifier

undefPrefix :: PreprocessingParserX ()
undefPrefix = nullifyParser $ octothorpe >> undef

undef :: PreprocessingParserX ()
undef = stringSatisfy_ (=="undef")