module Preprocessor.Constant where
import AbstractSyntaxTree
import CustomCombinators
import Preprocessor.CharacterConstant
import Preprocessor.Identifier
import qualified CharTokenParsers.PrimitiveParsers.IntegerConstant as CTP.IntegerConstant
import Preprocessor.PreprocessingParser
import Text.Parsec.Prim
    ( parserFail
    , (<|>)
    )


constant :: PreprocessingParserX Constant
constant =
    integerConstantConstant <|>
    floatingConstantConstant <|>
    enumerationConstantConstant <|>
    characterConstantConstant

integerConstantConstant :: PreprocessingParserX Constant
integerConstantConstant = simpleExpression integerConstant IntegerConstantConstant

floatingConstantConstant :: PreprocessingParserX Constant
floatingConstantConstant = simpleExpression floatingConstant FloatingConstantConstant

enumerationConstantConstant :: PreprocessingParserX Constant
enumerationConstantConstant = simpleExpression enumerationConstant EnumerationConstantConstant

characterConstantConstant :: PreprocessingParserX Constant
characterConstantConstant = simpleExpression characterConstant CharacterConstantConstant

integerConstant :: PreprocessingParserX IntegerConstant
integerConstant = charToStringTokenParser CTP.IntegerConstant.integerConstant

floatingConstant :: PreprocessingParserX FloatingConstant
floatingConstant = (parserFail "floatingConstant not implemented") {->> decimalFloatingConstant <|> hexadecimalFloatingConstant
-}

enumerationConstant :: PreprocessingParserX EnumerationConstant
enumerationConstant = simpleExpression identifier EnumerationConstant

