module CharTokenParsers.PrimitiveParsers.UniversalCharacterName where
import AbstractSyntaxTree
import ASTEquipped
import CustomCombinators
import Numeric
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String



universalCharacterName :: Parser UniversalCharacterName
universalCharacterName = parserFail "universalCharacterName not finished being implemented" --tryWithFailMessage "Universal Character Name" $ universalShort <|> universalLong

{-universalCharacterNameOutOfRangeError :: String
universalCharacterNameOutOfRangeError = "Universal Character Name not in range"


-- See constraints for universal-character-name in the C spec
isValidUniversalCharacterName :: Integer -> Bool
isValidUniversalCharacterName universalCharNum
    | inBannedRange = False
    | tooSmall = False
    | otherwise = True
    where
        inBannedRange = 0xD800 <= universalCharNum && universalCharNum <= 0xDFFF
        tooSmall = universalCharNum < 0x00A0 && (not $ elem universalCharNum otherAllowedCharacters)
        otherAllowedCharacters = [0x0024, 0x0040, 0x0060]

universalShort :: Parser UniversalCharacterName
universalShort = do
    parsedPrefix <- string "\\u"
    parsedHexQuad <- hexQuad
    let hexQuadInteger = toInteger parsedHexQuad
    let passed = isValidUniversalCharacterName hexQuadInteger
    if passed
        then return $ ShortUniversalCharacterName parsedHexQuad
        else fail universalCharacterNameOutOfRangeError

universalLong :: Parser UniversalCharacterName
universalLong = do
    parsedPrefix <- string "\\U"
    parsedHexQuad1 <- hexQuad
    parsedHexQuad2 <- hexQuad
    let hexQuadIntegerPart1 = toInteger parsedHexQuad1 * (16 ^ 4)
    let hexQuadIntegerPart2 = toInteger parsedHexQuad2
    let hexQuadInteger = hexQuadIntegerPart1 + hexQuadIntegerPart2
    let inRange = isValidUniversalCharacterName hexQuadInteger
    if inRange
        then return $ LongUniversalCharacterName parsedHexQuad1 parsedHexQuad2
        else fail universalCharacterNameOutOfRangeError
-}

hexQuad :: Parser HexQuad
hexQuad = parserFail "hexQuad not finished being implemented" {-do
    char1 <- HexadecimalDigit <$> hexDigit
    char2 <- HexadecimalDigit <$> hexDigit
    char3 <- HexadecimalDigit <$> hexDigit
    char4 <- HexadecimalDigit <$> hexDigit
    return $ HexQuad (char1, char2, char3, char4)
    -}

