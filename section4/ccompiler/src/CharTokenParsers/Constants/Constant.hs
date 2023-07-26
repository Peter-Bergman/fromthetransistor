module CharTokenParsers.Constants.Constant (constant) where
import AbstractSyntaxTree
import CustomCombinators
import qualified CharTokenParsers.Constants.IntegerConstants.IntegerConstant as CTP.IntegerConstant
import qualified CharTokenParsers.Constants.EnumerationConstants.EnumerationConstant as CTP.EnumerationConstant
import qualified CharTokenParsers.Constants.CharacterConstants.CharacterConstant as CTP.CharacterConstant
import Text.Parsec.Prim
    ( parserFail
    , (<|>)
    )
import Text.Parsec.String
    (Parser)


constant :: Parser Constant
constant =
    integerConstantConstant <|>
    floatingConstantConstant <|>
    characterConstantConstant <|> -- NOTE: characterConstantConstant has to come before enumerationConstantConstant i.e. "L'a'"
    enumerationConstantConstant

integerConstantConstant :: Parser Constant
integerConstantConstant = simpleExpression CTP.IntegerConstant.integerConstant IntegerConstantConstant

floatingConstantConstant :: Parser Constant
floatingConstantConstant = (parserFail "floatingConstant not implemented") --simpleExpression floatingConstant FloatingConstantConstant

enumerationConstantConstant :: Parser Constant
enumerationConstantConstant = simpleExpression CTP.EnumerationConstant.enumerationConstant EnumerationConstantConstant

characterConstantConstant :: Parser Constant
characterConstantConstant = simpleExpression CTP.CharacterConstant.characterConstant CharacterConstantConstant

