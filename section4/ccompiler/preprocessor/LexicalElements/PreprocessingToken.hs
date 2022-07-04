module PreprocessingToken where
import Constants.CharacterConstants.CharacterConstant
import Data.Char
import HeaderNames.HeaderName
import Identifiers.Identifier
import PPNumbers.PPNumber
import Punctuators.Punctuator
import StringLiterals.StringLiteral
import Text.ParserCombinators.Parsec
import Text.Parsec hiding (try)


preprocessingToken :: Parser String
preprocessingToken = 
    try (headerName) <|>
    try (identifier) <|>
    try (ppNumber) <|>
    try (characterConstant) <|>
    try (stringLiteral) <|>
    try (punctuator) <|>
    otherCharacter

otherCharacter :: Parser String
otherCharacter = do
    parsedCharacter <- satisfy nonSpace
    return $ parsedCharacter : ""
    
nonSpace :: Char -> Bool
nonSpace = not . isSpace
