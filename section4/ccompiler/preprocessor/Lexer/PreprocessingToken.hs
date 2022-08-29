module PreprocessingToken where
import Constants.CharacterConstants.CharacterConstant
import Data.Char
import HeaderNames.HeaderName
import Identifiers.Identifier
import PPNumbers.PPNumber
import Punctuators.Punctuator
import StringLiterals.StringLiteral
import Text.Parsec
import Text.Parsec.String


preprocessingToken :: Parser String
preprocessingToken = 
    try (headerName) <|>
    try (identifier) <|>
    try (ppNumber) <|>
    try (characterConstant) <|>
    try (stringLiteral) <|>
    try (punctuator) <|>
    newLineAsString <|>
    otherCharacter

otherCharacter :: Parser String
otherCharacter = do
    parsedCharacter <- satisfy nonSpace
    return $ parsedCharacter : ""

newLineAsString :: Parser String
newLineAsString = do
    parsedNewLineCharacter <- char '\n'
    return [ parsedNewLineCharacter ]
    
nonSpace :: Char -> Bool
nonSpace = not . isSpace
