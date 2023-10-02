module CharTokenParsers.HeaderNames.HeaderName (headerName) where
import AbstractSyntaxTree
import CustomCombinators
import Data.List
import Data.List.NonEmpty as NE
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

headerName :: Parser HeaderName
headerName = hHeaderName <|> qHeaderName <?> "Header Name"

hHeaderName :: Parser HeaderName
hHeaderName = do
    _ <- char '<'
    parsedHCharSequence <- hCharSequence
    if isSemanticallyValidHCharSequence parsedHCharSequence then return () else fail "Invalid Header File Name"
    _ <- char '>'
    return $ HHeaderName parsedHCharSequence

qHeaderName :: Parser HeaderName
qHeaderName = do
    _ <- char '\"'
    headerName <- qCharSequence
    if isSemanticallyValidQCharSequence headerName then return () else fail "Invalid Header File Name"
    _ <- char '\"'
    return $ QHeaderName headerName

hCharSequence :: Parser HCharSequence
hCharSequence = simpleExpression (many1NonEmpty hChar) HCharSequence

hChar :: Parser HChar
hChar = simpleExpression (noneOf ">\n") HChar

qCharSequence :: Parser QCharSequence
qCharSequence = simpleExpression (many1NonEmpty qChar) QCharSequence

qChar :: Parser QChar
qChar = simpleExpression (noneOf "\"\n") QChar

isSemanticallyValidHCharSequence :: HCharSequence -> Bool
isSemanticallyValidHCharSequence (HCharSequence nonEmptyHChars) = not $ anyElementsAreSubstringOfString semanticallyUnallowedHCharSequenceSymbols hCharSequenceAsString
    where
        hCharSequenceAsString = toList nonEmptyHChar
        nonEmptyHChar = NE.map hCharToChar nonEmptyHChars
        hCharToChar (HChar char) = char
        
isSemanticallyValidQCharSequence :: QCharSequence -> Bool
isSemanticallyValidQCharSequence (QCharSequence nonEmptyQChars) = not $ anyElementsAreSubstringOfString semanticallyUnallowedQCharSequenceSymbols qCharSequenceAsString
    where
        qCharSequenceAsString = toList nonEmptyQChar
        nonEmptyQChar = NE.map qCharToChar nonEmptyQChars
        qCharToChar (QChar char) = char

anyElementsAreSubstringOfString :: [String] -> String -> Bool
anyElementsAreSubstringOfString listOfStrings superString = any (`isInfixOf` superString) listOfStrings

semanticallyUnallowedQCharSequenceSymbols :: [String]
semanticallyUnallowedQCharSequenceSymbols = ["'", "\\", "//", "/*"]

semanticallyUnallowedHCharSequenceSymbols :: [String]
semanticallyUnallowedHCharSequenceSymbols = "\"" : semanticallyUnallowedQCharSequenceSymbols

