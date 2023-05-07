module ASTEquipped where
import AbstractSyntaxTree
import Data.Char
import Numeric

{-instance Enum HexQuad where
    toEnum hexadecimalQuadInt = error "toEnum for HexQuad not implemented. make me" case hexadecimalDigitInt of -- I will see if all this code matters, throwing error for now
        0  -> HexQuad $ '0'
        1  -> HexQuad $ '1'
        2  -> HexQuad $ '2'
        3  -> HexQuad $ '3'
        4  -> HexQuad $ '4'
        5  -> HexQuad $ '5'
        6  -> HexQuad $ '6'
        7  -> HexQuad $ '7'
        8  -> HexQuad $ '8'
        9  -> HexQuad $ '9'
        10 -> HexQuad $ 'a'
        11 -> HexQuad $ 'b'
        12 -> HexQuad $ 'c'
        13 -> HexQuad $ 'd'
        14 -> HexQuad $ 'e'
        15 -> HexQuad $ 'f'
        _  -> error $ (show hexadecimalDigitInt) ++ " is not a Hexadecimal Digit. It should not be allowed in the HexadecimalDigit type."
        where
            hexString = showHex hexadecimalDigitInt ""
            hexStringLength = length hexString
            tooBig = hexStringLength > 4
            leftPaddedHexString = if tooBig then error "fuck you, Int too big for HexQuad" else -}

    {-fromEnum (HexQuad ( = case getHexDigitChar hexadecimalDigit of
        '0' -> 0
        '1' -> 1
        '2' -> 2
        '3' -> 3
        '4' -> 4
        '5' -> 5
        '6' -> 6
        '7' -> 7
        '8' -> 8
        '9' -> 9
        'a' -> 10
        'b' -> 11
        'c' -> 12
        'd' -> 13
        'e' -> 14
        'f' -> 15
        _ -> error $ (getHexDigitChar $ lowerCaseHexadecimalDigit hexadecimalDigit) : " is not a Hexadecimal Digit. It should not be allowed in the HexadecimalDigit type."
        where
            lowerCaseHexadecimalDigit = HexadecimalDigit . toLower . getHexDigitChar
            getHexDigitChar (HexadecimalDigit hexadecimalDigitChar) = hexadecimalDigitChar
-}

{-instance Integral HexQuad where
    quotRem a b = (a,b) -- arbitrary definition, do not use
    {-quotRem (toInteger a) (toInteger b) -- had trouble defining, not implementing for now
        where
            fromInteger' integer = HexQuad (HexadecimalDigit $ firstDigit integer, HexadecimalDigit $ secondDigit integer, HexadecimalDigit $ thirdDigit integer, HexadecimalDigit $ fourthDigit integer)
            firstDigit   integer = ord $ integer `mod` (16 ^ 3)
            secondDigit  integer = ord $ integer `mod` (16 ^ 2)
            thirdDigit   integer = ord $ integer `mod` (16 ^ 1)
            fourthDigit  integer = ord $ integer `mod` (16 ^ 0)-}
            
    toInteger (HexQuad (a,b,c,d)) = aScaled + bScaled + cScaled + dScaled
        where
            aScaled = (toInteger a) * (16 ^ 3)
            bScaled = (toInteger b) * (16 ^ 2)
            cScaled = (toInteger c) * (16 ^ 1)
            dScaled = (toInteger d) * (16 ^ 0)
-}

{-instance Integral HexadecimalDigit where
    quotRem a b = error "not implemented yet, too much time to give a fuck yet"--quotRem (toInteger a) (toInteger b) -- had trouble defining, not implementing for now
    toInteger hexadecimalDigit = case getHexDigitChar hexadecimalDigit of
        '0' -> 0
        '1' -> 1
        '2' -> 2
        '3' -> 3
        '4' -> 4
        '5' -> 5
        '6' -> 6
        '7' -> 7
        '8' -> 8
        '9' -> 9
        'a' -> 10
        'b' -> 11
        'c' -> 12
        'd' -> 13
        'e' -> 14
        'f' -> 15
        _ -> error $ (getHexDigitChar $ lowerCaseHexadecimalDigit hexadecimalDigit) : " is not a Hexadecimal Digit. It should not be allowed in the HexadecimalDigit type."
        where
            lowerCaseHexadecimalDigit = HexadecimalDigit . toLower . getHexDigitChar
            getHexDigitChar (HexadecimalDigit hexadecimalDigitChar) = hexadecimalDigitChar
-}



leftPadZeros :: Int -> String -> String
leftPadZeros width str = replicate (width - length str) '0' ++ str

