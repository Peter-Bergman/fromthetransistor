module CharTokenParsers.PrimitiveParsers.IntegerConstant
( hexadecimalConstant
, hexadecimalDigit
, hexadecimalPrefix
, integerConstant
)
where
import AbstractSyntaxTree
import CharTokenParsers.PrimitiveParsers.HexadecimalDigit
    (hexadecimalDigit)
import CustomCombinators
import Data.Char
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String


integerConstant :: Parser IntegerConstant
integerConstant =
    decimalConstantIntegerConstant <|>
    octalConstantIntegerConstant <|>
    hexadecimalConstantIntegerConstant

decimalConstantIntegerConstant :: Parser IntegerConstant
decimalConstantIntegerConstant = try $ do
    parsedDecimalConstant <- decimalConstant
    parsedMaybeIntegerSuffix <- tryMaybe $ integerSuffix
    return $ DecimalConstantIntegerConstant parsedDecimalConstant parsedMaybeIntegerSuffix

octalConstantIntegerConstant :: Parser IntegerConstant
octalConstantIntegerConstant = try $ do
    parsedOctalConstant <- octalConstant
    parsedMaybeIntegerSuffix <- tryMaybe $ integerSuffix
    return $ OctalConstantIntegerConstant parsedOctalConstant parsedMaybeIntegerSuffix

hexadecimalConstantIntegerConstant :: Parser IntegerConstant
hexadecimalConstantIntegerConstant = try $ do
    parsedHexadecimalConstant <- hexadecimalConstant
    parsedMaybeIntegerSuffix <- tryMaybe $ integerSuffix
    return $ HexadecimalConstantIntegerConstant parsedHexadecimalConstant parsedMaybeIntegerSuffix

decimalConstant :: Parser DecimalConstant
decimalConstant = tryWithFailMessage "Decimal Constant" $ do
    parsedNonZeroDigit <- nonzeroDigit
    parsedDigitChars <- many digit
    let parsedDigits = map (\character -> Digit $ read $ character : "") parsedDigitChars
    return $ DecimalConstant parsedNonZeroDigit parsedDigits

nonzeroDigit :: Parser NonzeroDigit
nonzeroDigit = try $ do
    parsedDigitChar <- notFollowedBy (char '0') >> digit <?> "Nonzero Digit"
    return $ NonzeroDigit $ read $ parsedDigitChar : ""

-- test this
octalConstant :: Parser OctalConstant
octalConstant = tryWithFailMessage "Octal Constant" $ do
    _ <- char '0'
    parsedOctalDigitChars <- many octDigit
    let parsedOctalDigits = map (\octalDigitCharacter -> OctalDigit $ read $ octalDigitCharacter : "") parsedOctalDigitChars
    return $ OctalConstant parsedOctalDigits

hexadecimalConstant :: Parser HexadecimalConstant
hexadecimalConstant = tryWithFailMessage "Hexadecimal Constant" $ do
    parsedHexadecimalPrefix <- hexadecimalPrefix
    parsedHexadecimalDigits <- many1NonEmpty hexadecimalDigit
    return $ HexadecimalConstant parsedHexadecimalPrefix parsedHexadecimalDigits

hexadecimalPrefix :: Parser HexadecimalPrefix
hexadecimalPrefix = capitalXHexPrefix <|> lowerCaseXHexPrefix

capitalXHexPrefix :: Parser HexadecimalPrefix
capitalXHexPrefix = try $ string "0X" >> return CapitalXHexPrefix

lowerCaseXHexPrefix :: Parser HexadecimalPrefix
lowerCaseXHexPrefix = try $ string "0x" >> return LowerCaseXHexPrefix

--hexadecimalDigit :: Parser HexadecimalDigit
--hexadecimalDigit = hexDigit >>= return . HexadecimalDigit

-- moved integerSuffix and subtree to IntegerConstant.hs
-- still need to refactor into char token parser
integerSuffix :: Parser IntegerSuffix
integerSuffix =
    unsignedMaybeLong <|> -- check order for this
    unsignedLongLong <|>
    longMaybeUnsigned <|>
    longLongMaybeUnsigned

unsignedMaybeLong :: Parser IntegerSuffix
unsignedMaybeLong = try $ do
    parsedUnsignedSuffix <- unsignedSuffix
    parsedLongSuffix <- tryMaybe $ longSuffix
    return $ UnsignedMaybeLong parsedUnsignedSuffix parsedLongSuffix

unsignedLongLong :: Parser IntegerSuffix
unsignedLongLong = try $ do
    parsedUnsignedSuffix <- unsignedSuffix
    parsedLongLongSuffix <- longLongSuffix
    return $ UnsignedLongLong parsedUnsignedSuffix parsedLongLongSuffix

longMaybeUnsigned :: Parser IntegerSuffix
longMaybeUnsigned = try $ do
    parsedLongSuffix <- longSuffix
    parsedMaybeUnsignedSuffix <- tryMaybe $ unsignedSuffix
    return $ LongMaybeUnsigned parsedLongSuffix parsedMaybeUnsignedSuffix

longLongMaybeUnsigned :: Parser IntegerSuffix
longLongMaybeUnsigned = try $ do
    parsedLongLongSuffix <- longLongSuffix
    parsedMaybeUnsignedSuffix <- tryMaybe $ unsignedSuffix
    return $ LongLongMaybeUnsigned parsedLongLongSuffix parsedMaybeUnsignedSuffix

unsignedSuffix :: Parser UnsignedSuffix
unsignedSuffix = capitalUUnsignedSuffix <|> lowerCaseUUnsignedSuffix

capitalUUnsignedSuffix :: Parser UnsignedSuffix
capitalUUnsignedSuffix = string "U" >> return CapitalUUnsignedSuffix

lowerCaseUUnsignedSuffix :: Parser UnsignedSuffix
lowerCaseUUnsignedSuffix = string "u" >> return LowerCaseUUnsignedSuffix

longSuffix :: Parser LongSuffix
longSuffix = lowerCaseLLongSuffix <|> capitalLLongSuffix

lowerCaseLLongSuffix :: Parser LongSuffix
lowerCaseLLongSuffix = string "l" >> return LowerCaseLLongSuffix

capitalLLongSuffix :: Parser LongSuffix
capitalLLongSuffix = string "L" >> return CapitalLLongSuffix

longLongSuffix :: Parser LongLongSuffix
longLongSuffix = lowerCaseLLongLongSuffix <|> capitalLLongLongSuffix

lowerCaseLLongLongSuffix :: Parser LongLongSuffix
lowerCaseLLongLongSuffix = string "ll" >> return LowerCaseLLongLongSuffix

capitalLLongLongSuffix :: Parser LongLongSuffix
capitalLLongLongSuffix = string "LL" >> return CapitalLLongLongSuffix

