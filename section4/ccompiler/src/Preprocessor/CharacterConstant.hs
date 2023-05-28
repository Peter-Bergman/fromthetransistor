module Preprocessor.CharacterConstant
( characterConstant
--, cCharSequence
--, cChar
, escapeSequence
, simpleEscapeSequence
, octalEscapeSequence
, hexadecimalEscapeSequence
, universalCharacterNameEscapeSequence
) where
import AbstractSyntaxTree
import CharTokenParsers.PrimitiveParsers.UniversalCharacterName
import qualified CharTokenParsers.Constants.CharacterConstants.CharacterConstant as CTP.CharacterConstant
import CharTokenParsers.PrimitiveParsers.EscapeSequence
import CustomCombinators
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Prim
import Text.Parsec.Combinator

characterConstant :: Parser CharacterConstant
characterConstant =
    simpleCharacterConstant <|>
    lCharacterConstant <|>
    lowerCaseUCharacterConstant <|>
    capitalUCharacterConstant

simpleCharacterConstant :: Parser CharacterConstant
simpleCharacterConstant = cCharSequenceBetweenSingleQuotes >>= return . SimpleCharacterConstant

cCharSequenceBetweenSingleQuotes :: Parser CCharSequence
cCharSequenceBetweenSingleQuotes = between singleQuote singleQuote CTP.CharacterConstant.cCharSequence

lCharacterConstant :: Parser CharacterConstant
lCharacterConstant = char 'L' >> cCharSequenceBetweenSingleQuotes >>= return . LCharacterConstant

lowerCaseUCharacterConstant :: Parser CharacterConstant
lowerCaseUCharacterConstant = char 'u' >> cCharSequenceBetweenSingleQuotes >>= return . LowerCaseUCharacterConstant

capitalUCharacterConstant :: Parser CharacterConstant
capitalUCharacterConstant = char 'U' >> cCharSequenceBetweenSingleQuotes >>= return . CapitalUCharacterConstant

singleQuote :: Parser Char
singleQuote = char '\''

