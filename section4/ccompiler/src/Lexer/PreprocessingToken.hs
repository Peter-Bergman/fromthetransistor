module Lexer.PreprocessingToken 
( preprocessingToken
, headerName
, identifier
, characterConstant
, stringLiteral
, ppNumber
, punctuator
, otherCharacter
) where
import CharTokenParsers.Constants.CharacterConstants.CharacterConstant
import CharTokenParsers.HeaderNames.HeaderName
import CharTokenParsers.Identifiers.Identifier
import CharTokenParsers.PPNumbers.PPNumber
import CharTokenParsers.Punctuators.Punctuator
import CharTokenParsers.StringLiterals.StringLiteral
import Data.Char
import Text.Parsec
import Text.Parsec.String


preprocessingToken :: Parser String
preprocessingToken = 
    try (headerName) <|>
    try (characterConstant) <|>
    try (identifier) <|>
    try (ppNumber) <|>
    try (stringLiteral) <|>
    try (punctuator) <|>
    otherCharacter

otherCharacter :: Parser String
otherCharacter = do
    parsedCharacter <- satisfy nonSpace
    return $ parsedCharacter : ""
    
nonSpace :: Char -> Bool
nonSpace = not . isSpace

