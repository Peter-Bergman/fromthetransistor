module CharTokenParsers.PrimitiveParsers.UniversalCharacterName where
import Numeric
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String



-- Universal Character Name Parser Section

universalCharacterName :: Parser String
universalCharacterName = try (universalShort) <|> universalLong

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

universalShort :: Parser String
universalShort = do
    prefix <- string "\\u"
    hexQuad <- hexQuadParser
    let hexQuadInteger = fst $ head $ readHex hexQuad
    let passed = universalCharacterInRange hexQuadInteger
    if passed
        then return $ prefix ++ hexQuad
        else fail universalCharacterNameOutOfRangeError

universalLong :: Parser String
universalLong = do
    prefix <- string "\\U"
    hexQuad1 <- hexQuadParser
    hexQuad2 <- hexQuadParser
    let hexQuadIntegerPart1 = (fst $ head $ readHex hexQuad1 ) * (16 * 16 * 16 * 16)
    let hexQuadIntegerPart2 = fst $ head $ readHex hexQuad2
    let hexQuadInteger = hexQuadIntegerPart1 + hexQuadIntegerPart2
    if ( universalCharacterInRange hexQuadInteger )
        then return $ prefix ++ hexQuad1 ++ hexQuad2
        else fail universalCharacterNameOutOfRangeError

hexQuadParser :: Parser String
hexQuadParser = do
    char1 <- hexDigit
    char2 <- hexDigit
    char3 <- hexDigit
    char4 <- hexDigit
    return ([char1] ++ [char2] ++ [char3] ++ [char4])

-- End of Universal Character Parser Section

