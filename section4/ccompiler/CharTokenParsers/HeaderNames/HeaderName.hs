module CharTokenParsers.HeaderNames.HeaderName (headerName) where
import Data.List
import System.Environment
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

headerName :: Parser String
headerName = do
    let hHeaderName = do
        char '<'
        headerName <- hCharSequence
        if isSemanticallyValidHCharSequence headerName then return () else fail "Invalid Header File Name"
        char '>'
        return $ "<" ++ headerName ++ ">"
    let qHeaderName = do
        char '\"'
        headerName <- qCharSequence
        if isSemanticallyValidQCharSequence headerName then return () else fail "Invalid Header File Name"
        char '\"'
        return $ "\"" ++ headerName ++ "\""
    (hHeaderName <|> qHeaderName) <?> "Header Name"

hCharSequence :: Parser String
hCharSequence = many hChar

hChar :: Parser Char
hChar = noneOf ">\n"

qCharSequence :: Parser String
qCharSequence = many qChar

qChar :: Parser Char
qChar = noneOf "\"\n"

isSemanticallyValidHCharSequence :: String -> Bool
isSemanticallyValidHCharSequence inputHCharSequence = not $ anyElementsAreSubstringOfString semanticallyUnallowedHCharSequenceSymbols inputHCharSequence
--not $ any (`isInfixOf` inputHCharSequence) semanticallyUnallowedHCharSequenceSymbols

isSemanticallyValidQCharSequence :: String -> Bool
isSemanticallyValidQCharSequence inputQCharSequence = not $ anyElementsAreSubstringOfString semanticallyUnallowedQCharSequenceSymbols inputQCharSequence

anyElementsAreSubstringOfString :: [String] -> String -> Bool
anyElementsAreSubstringOfString listOfStrings superString = any (`isInfixOf` superString) listOfStrings

semanticallyUnallowedQCharSequenceSymbols :: [String]
semanticallyUnallowedQCharSequenceSymbols = ["'", "\\", "//", "/*"]

semanticallyUnallowedHCharSequenceSymbols :: [String]
semanticallyUnallowedHCharSequenceSymbols = "\"" : semanticallyUnallowedQCharSequenceSymbols

