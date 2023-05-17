module Lexer.PreprocessingToken
( preprocessingToken
, headerNamePreprocessingToken
, identifierPreprocessingToken
, characterConstantPreprocessingToken
, stringLiteralPreprocessingToken
, ppNumberPreprocessingToken
, punctuatorPreprocessingToken
, otherCharacterPreprocessingToken
) where
import AbstractSyntaxTree
import CharTokenParsers.Constants.CharacterConstants.CharacterConstant
import CharTokenParsers.HeaderNames.HeaderName
import CharTokenParsers.Identifiers.Identifier
import CharTokenParsers.PPNumbers.PPNumber
import CharTokenParsers.Punctuators.Punctuator
import CharTokenParsers.StringLiterals.StringLiteral
import CustomCombinators
import Data.Char
import Text.Parsec
import Text.Parsec.String


preprocessingToken :: Parser PreprocessingToken
preprocessingToken =
    try (headerNamePreprocessingToken) <|>
    try (characterConstantPreprocessingToken) <|>
    try (identifierPreprocessingToken) <|>
    try (ppNumberPreprocessingToken) <|>
    try (stringLiteralPreprocessingToken) <|>
    try (punctuatorPreprocessingToken) <|>
    otherCharacterPreprocessingToken

headerNamePreprocessingToken :: Parser PreprocessingToken
headerNamePreprocessingToken = simpleExpression headerName HeaderNamePreprocessingToken

characterConstantPreprocessingToken :: Parser PreprocessingToken
characterConstantPreprocessingToken = simpleExpression characterConstant CharacterConstantPreprocessingToken

identifierPreprocessingToken :: Parser PreprocessingToken
identifierPreprocessingToken = simpleExpression identifier IdentifierPreprocessingToken

ppNumberPreprocessingToken :: Parser PreprocessingToken
ppNumberPreprocessingToken = simpleExpression ppNumber PPNumberPreprocessingToken

stringLiteralPreprocessingToken :: Parser PreprocessingToken
stringLiteralPreprocessingToken = simpleExpression stringLiteral StringLiteralPreprocessingToken

punctuatorPreprocessingToken :: Parser PreprocessingToken
punctuatorPreprocessingToken = simpleExpression punctuator PunctuatorPreprocessingToken

otherCharacterPreprocessingToken :: Parser PreprocessingToken
otherCharacterPreprocessingToken = do
    parsedCharacter <- satisfy nonSpace
    return $ OtherCharacterPreprocessingToken parsedCharacter

nonSpace :: Char -> Bool
nonSpace = not . isSpace

