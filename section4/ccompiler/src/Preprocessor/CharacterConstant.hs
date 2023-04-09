module Preprocessor.CharacterConstant
( characterConstant
, cCharSequence
, cChar
, escapeSequence
, simpleEscapeSequence
, octalEscapeSequence
, hexadecimalEscapeSequence
, universalCharacterNameEscapeSequence
) where
import AbstractSyntaxTree
import CharTokenParsers.PrimitiveParsers.UniversalCharacterName
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
cCharSequenceBetweenSingleQuotes = between singleQuote singleQuote cCharSequence

lCharacterConstant :: Parser CharacterConstant
lCharacterConstant = char 'L' >> cCharSequenceBetweenSingleQuotes >>= return . LCharacterConstant

lowerCaseUCharacterConstant :: Parser CharacterConstant
lowerCaseUCharacterConstant = char 'u' >> cCharSequenceBetweenSingleQuotes >>= return . LowerCaseUCharacterConstant

capitalUCharacterConstant :: Parser CharacterConstant
capitalUCharacterConstant = char 'U' >> cCharSequenceBetweenSingleQuotes >>= return . CapitalUCharacterConstant

cCharSequence :: Parser CCharSequence
cCharSequence = many1NonEmpty cChar

cChar :: Parser CChar
cChar = escapeSequenceCChar <|> simpleCChar

simpleCChar :: Parser CChar
simpleCChar = satisfy (\character -> character /='\'' && character /= '\\' && character /= '\n')

escapeSequence :: Parser EscapeSequence
escapeSequence = simpleEscapeSequence <|> octalEscapeSequence <|> hexadecimalEscapeSequence <|> universalCharacterNameEscapeSequence

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
