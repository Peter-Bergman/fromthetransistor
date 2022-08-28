module Punctuators.Punctuator where
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Text

punctuator :: Parser String
punctuator = 
    {-
      The order here is different than in the ISO document.
      This is because of precedence.
      The "&" string will be parsed before "&&" which is not correct.
    -}
    string "[" <|>
    string "]" <|>
    string "(" <|>
    string ")" <|>
    string "{" <|>
    string "}" <|>
    try (string "...") <|>
    string "." <|>
    try (string "->") <|>
    try (string "++") <|>
    try (string "--") <|>
    try (string "*=") <|>
    try (string "/=") <|>
    try (string "%=") <|>
    try (string "+=") <|>
    try (string "-=") <|>
    try (string "<<=") <|>
    try (string ">>=") <|>
    try (string "&=") <|>
    try (string "^=") <|>
    try (string "|=") <|>
    try (string "<=") <|>
    try (string ">=") <|>
    try (string "==") <|>
    try (string "!=") <|>
    try (string "<:") <|>
    try (string ":>") <|>
    try (string "<%") <|>
    try (string "%>") <|>
    try (string "%:%:") <|>
    try (string "%:") <|>
    try (string "&&") <|>
    try (string "||") <|>
    string "&" <|>
    string "*"<|>
    string "+" <|>
    string "-" <|>
    string "~" <|>
    string "!" <|>
    string "/" <|>
    string "%" <|>
    try (string "<<") <|>
    try (string ">>") <|>
    string "<" <|>
    string ">" <|>
    string "^" <|>
    string "|" <|>
    string "?" <|>
    string ":" <|>
    string ";" <|>
    string "="
    <?> "Punctuator"


