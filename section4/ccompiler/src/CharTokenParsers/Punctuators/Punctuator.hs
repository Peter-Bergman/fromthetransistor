module CharTokenParsers.Punctuators.Punctuator where
import AbstractSyntaxTree
import CustomCombinators
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String

punctuator :: Parser Punctuator
punctuator = tryWithFailMessage "Punctuator" $ do
    {-
      The order here is different than in the ISO document.
      This is because of precedence.
      The "&" string will be parsed before "&&" which is not correct.
    -}
    parsedPunctuatorString <-
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
    return $ Punctuator parsedPunctuatorString

