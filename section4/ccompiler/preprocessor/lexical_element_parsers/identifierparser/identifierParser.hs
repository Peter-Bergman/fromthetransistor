module Main where
import Data.List
import Numeric
import System.Environment
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

main :: IO()
main = do
    args <- getArgs
    let firstArg = args !! 0
    putStrLn (readExpr (firstArg))

mainParser = identifierParser

readExpr :: String -> String
readExpr input = case parse (mainParser) "identifierParser" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val


type Identifier = String


identifierParser :: Parser Identifier
identifierParser = do
    let digitString = do
        digitParsed <- digit
        return [digitParsed]
    firstCharacter <- identifierNonDigitParser
    remainingCharacters <- many (digitString <|> identifierNonDigitParser)
    let identifier = firstCharacter ++ (intercalate "" remainingCharacters)
    return identifier


identifierNonDigitParser :: Parser String
identifierNonDigitParser = nonDigitParser <|> universalCharacterNameParser

nonDigitParser :: Parser String
nonDigitParser = do
    character <- letter <|> char '_'
    return [character]



-- Universal Character Name Parser Section

universalCharacterNameParser :: Parser String
universalCharacterNameParser = try (universalShort) <|> universalLong

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


