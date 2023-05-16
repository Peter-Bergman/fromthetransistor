module CharTokenParsers.Constants.CharacterConstants.CharacterConstant where
import AbstractSyntaxTree
import CustomCombinators
import Data.List
import Data.List.NonEmpty
import Numeric
import CharTokenParsers.PrimitiveParsers.UniversalCharacterName
import CharTokenParsers.PrimitiveParsers.EscapeSequence
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String


characterConstant :: Parser CharacterConstant
characterConstant = tryWithFailMessage "Character Constant" $ do
    parsedCharacterPrefix <- characterPrefix
    let characterConstantConstructor = determineCharacterConstantConstructorFromPrefix parsedCharacterPrefix
    parsedCCharSequence <- singleQuoted cCharSequence
    return $ characterConstantConstructor parsedCCharSequence

characterPrefix :: Parser (Maybe Char)
characterPrefix = optionMaybe $ oneOf "LuU"

determineCharacterConstantConstructorFromPrefix :: Maybe Char -> (CCharSequence -> CharacterConstant)
determineCharacterConstantConstructorFromPrefix prefix =
    case prefix of
        Nothing -> SimpleCharacterConstant
        Just 'L' -> LCharacterConstant
        Just 'u' -> LowerCaseUCharacterConstant
        Just 'U' -> CapitalUCharacterConstant

singleQuoted :: Parser a -> Parser a
singleQuoted = between (string "'") (string "'")

cCharSequence :: Parser CCharSequence
cCharSequence = do
    parsedCChars <- many1 cChar
    return $ CCharSequence $ fromList parsedCChars

cChar :: Parser CChar
cChar = nonEscapeSequenceCChar <|> escapeSequenceCChar

escapeSequenceCChar :: Parser CChar
escapeSequenceCChar = simpleExpression escapeSequence EscapeSequenceCChar

nonEscapeSequenceCChar :: Parser CChar
nonEscapeSequenceCChar = do
    parsedChar <- noneOf $ '\'' : '\\' : "\n"
    return $ NonEscapeSequenceCChar parsedChar

