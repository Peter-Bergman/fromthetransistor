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


{-characterConstant :: Parser String
characterConstant = (do
    prefix <- characterPrefix
    openingQuote <- char '\''
    parsedCCharSequence <- cCharSequence
    closingQuote <- char '\'' <?> "closing quote"
    return $ prefix ++ [openingQuote] ++  parsedCCharSequence ++ [closingQuote]) <?> "Character Constant"-}

characterConstant :: Parser CharacterConstant
characterConstant = tryWithFailMessage "Character Constant" $ do
    parsedCharacterPrefix <- characterPrefix
    let characterConstantConstructor = determineCharacterConstantConstructorFromPrefix parsedCharacterPrefix
    parsedCCharSequence <- singleQuoted cCharSequence
    return $ characterConstantConstructor parsedCCharSequence


{-characterPrefix :: Parser String
characterPrefix = do
    -- 'a' is just an arbitrary character here
    -- Any non 'L', 'u', or 'U' character would work in its place.
    let alternativeCharacter = 'a'
    let optionalPrefix = option alternativeCharacter (oneOf "LuU")
    prefixChar <- optionalPrefix
    let parsedPrefix = if prefixChar == alternativeCharacter
        then ""
        else [prefixChar]
    return parsedPrefix-}

characterPrefix :: Parser (Maybe Char)
characterPrefix = optionMaybe $ oneOf "LuU"

determineCharacterConstantConstructorFromPrefix :: Maybe Char -> (CCharSequence -> CharacterConstant)
determineCharacterConstantConstructorFromPrefix prefix =
    case prefix of
        Nothing -> SimpleCharacterConstant
        Just 'L' -> LCharacterConstant
        Just 'u' -> LowerCaseUCharacterConstant
        Just 'U' -> CapitalUCharacterConstant


singleQuoted :: Parser String -> Parser String
singleQuoted = between (string "'") (string "'")

{-cCharSequence :: Parser String
cCharSequence = do
    firstCharacter <- cChar
    restOfCharactersList <- many cChar
    let restOfCharacters = intercalate "" restOfCharactersList
    return $ firstCharacter ++ restOfCharacters-}

cCharSequence :: Parser CCharSequence
cCharSequence = do
    parsedCChars <- many1 cChar
    return $ CCharSequence $ fromList parsedCChars

cChar :: Parser String
cChar = nonEscapeSequence <|> escapeSequence

nonEscapeSequence :: Parser String
nonEscapeSequence = do
    parsedCharacter <- noneOf $ '\'' : '\\' : "\n"
    return [parsedCharacter]

escapeSequence :: Parser EscapeSequence
escapeSequence =
    simpleEscapeSequenceEscapeSequence <|>
    octalEscapeSequenceEscapeSequence <|>
    hexadecimalEscapeSequenceEscapeSequence <|>
    universalCharacterNameEscapeSequence

simpleEscapeSequence :: Parser EscapeSequence
simpleEscapeSequence = anyOf 
    [ string "\\\'"
    , string "\\\""
    , string "\\?"
    , string "\\\\"
    , string "\\a"
    , string "\\b"
    , string "\\f"
    , string "\\n"
    , string "\\r"
    , string "\\t"
    , string "\\v"
    ]

octalEscapeSequenceEscapeSequence :: Parser OctalEscapeSequenceEscapeSequence
octalEscapeSequenceEscapeSequence = simpleExpression octalEscapeSequence OctalEscapeSequenceEscapeSequence

octalEscapeSequence :: Parser OctalEscapeSequence
octalEscapeSequence =
    tripleOctalEscapeSequence <|>
    doubleOctalEscapeSequence <|>
    singleOctalEscapeSequence

singleOctalEscapeSequence :: Parser OctalEscapeSequence
singleOctalEscapeSequence = try $ simpleExpression (char '\\' >> octalDigit) SingleOctalEscapeSequence

doubleOctalEscapeSequence :: Parser OctalEscapeSequence
doubleOctalEscapeSequence = try $ do
    char '\\'
    parsedFirstOctalDigit <- octalDigit
    parsedSecondOctalDigit <- octalDigit
    return $ DoubleOctalEscapeSequence parsedFirstOctalDigit parsedSecondOctalDigit

tripleOctalEscapeSequence :: Parser OctalEscapeSequence
tripleOctalEscapeSequence = try $ do
    char '\\'
    parsedFirstOctalDigit <- octalDigit
    parsedSecondOctalDigit <- octalDigit
    parsedThirdOctalDigit <- octalDigit
    return $ TripleOctalEscapeSequence parsedFirstOctalDigit parsedSecondOctalDigit parsedThirdOctalDigit


