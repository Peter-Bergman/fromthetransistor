module HeaderNames.HeaderName where
import System.Environment
import Text.Parsec.Char
import Text.ParserCombinators.Parsec

headerName = do
    let hHeaderName = do
        char '<'
        headerName <- hCharSequence
        char '>'
        return headerName
    let qHeaderName = do
        char '\"'
        headerName <- qCharSequence
        char '\"'
        return headerName
    hHeaderName <|> qHeaderName
        


hCharSequence :: Parser String
hCharSequence = many hChar

hChar :: Parser Char
hChar = noneOf ">\n"

qCharSequence :: Parser String
qCharSequence = many qChar

qChar :: Parser Char
qChar = noneOf "\"\n"
