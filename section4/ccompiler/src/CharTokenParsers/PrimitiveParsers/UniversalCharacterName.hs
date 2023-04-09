module CharTokenParsers.PrimitiveParsers.UniversalCharacterName where
import AbstractSyntaxTree
import CustomCombinators
import Numeric
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String



-- Universal Character Name Parser Section

universalCharacterName :: Parser UniversalCharacterName
universalCharacterName = tryWithFailMessage "Universal Character Name" $ universalShort <|> universalLong

universalCharacterNameOutOfRangeError :: String
universalCharacterNameOutOfRangeError = "Universal Character Name not in range"

universalCharacterInRange :: Integer -> Bool
universalCharacterInRange universalCharNum
    | 0xD800 <= universalCharNum && universalCharNum <= 0xDFFF = False
    | ( universalCharNum < 0x00A0 ) && not ( elem universalCharNum otherAllowedCharacters ) = False
    | otherwise = True
    where
        -- See ISO constraints for universal-character-name
        otherAllowedCharacters = [0x0024, 0x0040, 0x0060]

universalShort :: Parser UniversalCharacterName
universalShort = do
    prefix <- string "\\u"
    parsedHexQuad <- hexQuad
    let hexQuadInteger = fst $ head $ readHex parsedHexQuad
    let passed = universalCharacterInRange hexQuadInteger
    if passed
        then return $ UniversalCharacterName parsedHexQuad
        else fail universalCharacterNameOutOfRangeError

universalLong :: Parser String
universalLong = do
    prefix <- string "\\U"
    parsedHexQuad1 <- hexQuad
    parsedHexQuad2 <- hexQuad
    let hexQuadIntegerPart1 = (fst $ head $ readHex parsedHexQuad1) * (16 * 16 * 16 * 16)
    let hexQuadIntegerPart2 = fst $ head $ readHex parsedHexQuad2
    let hexQuadInteger = hexQuadIntegerPart1 + hexQuadIntegerPart2
    let inRange = universalCharacterInRange hexQuadInteger
    if inRange
        then return $ LongUniversalCharacterName hexQuad1 hexQuad2
        else fail universalCharacterNameOutOfRangeError

hexQuad :: Parser String
hexQuad = do
    char1 <- HexadecimalDigit <$> hexDigit
    char2 <- HexadecimalDigit <$> hexDigit
    char3 <- HexadecimalDigit <$> hexDigit
    char4 <- HexadecimalDigit <$> hexDigit
    return ([char1] ++ [char2] ++ [char3] ++ [char4])

-- End of Universal Character Parser Section

