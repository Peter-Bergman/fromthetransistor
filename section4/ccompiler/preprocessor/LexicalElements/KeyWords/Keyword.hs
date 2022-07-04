module Main where
import System.Environment
import Text.ParserCombinators.Parsec

main :: IO()
main = do
    args <- getArgs
    let firstArg = args !! 0
    putStrLn (readExpr (firstArg))

mainParser = keywordParser

readExpr :: String -> String
readExpr input = case parse (mainParser) "keyword" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

data Keyword = 
    Auto |
    Break |
    Case |
    Char |
    Const |
    Continue |
    Default |
    Do |
    Double |
    Else |
    Enum |
    Extern |
    Float | 
    For |
    Goto |
    If |
    Inline |
    Int |
    Long |
    Register |
    Restrict |
    Return |
    Short |
    Signed |
    Sizeof |
    Static |
    Struct |
    Switch |
    Typedef |
    Union |
    Unsigned |
    Void |
    Volatile |
    While |
    Alignas |
    Alignof |
    Atomic |
    Bool |
    Complex |
    Generic |
    Imaginary |
    NoReturn |
    StaticAssert |
    ThreadLocal

instance Show Keyword where
    show (Auto) = "auto"
    show (Break) = "break"
    show (Case) = "case"
    show (Char) = "char"
    show (Const) = "const"
    show (Continue) = "continue"
    show (Default) = "default"
    show (Do) = "do"
    show (Double) = "double"
    show (Else) = "else"
    show (Enum) = "enum"
    show (Extern) = "extern"
    show (Float) = "float"
    show (For) = "for"
    show (Goto) = "goto"
    show (If) = "if"
    show (Inline) = "inline"
    show (Int) = "int"
    show (Long) = "long"
    show (Register) = "register"
    show (Restrict) = "restrict"
    show (Return) = "return"
    show (Short) = "short"
    show (Signed) = "signed"
    show (Sizeof) = "sizeof"
    show (Static) = "static"
    show (Struct) = "struct"
    show (Switch) = "switch"
    show (Typedef) = "typedef"
    show (Union) = "union"
    show (Unsigned) = "unsigned"
    show (Void) = "void"
    show (Volatile) = "volatile"
    show (While) = "while"
    show (Alignas) = "_Alignas"
    show (Alignof) = "_Alignof"
    show (Atomic) = "_Atomic"
    show (Bool) = "_Bool"
    show (Complex) = "_Complex"
    show (Generic) = "_Generic"
    show (Imaginary) = "_Imaginary"
    show (NoReturn) = "_No_return"
    show (StaticAssert) = "_Static_assert"
    show (ThreadLocal) = "_Thread_local"
    

--type Keyword = String
keywordParser :: Parser Keyword
keywordParser = do
    let s = string
    keyword <- do {
        try (s "auto") <|>
        try (s "break") <|>
        try (s "case") <|>
        try (s "char") <|>
        try (s "const") <|>
        try (s "continue") <|>
        try (s "default") <|>
        try (s "do") <|>
        try (s "double") <|>
        try (s "else") <|>
        try (s "enum") <|>
        try (s "extern") <|>
        try (s "float") <|>
        try (s "for") <|>
        try (s "goto") <|>
        try (s "if") <|>
        try (s "inline") <|>
        try (s "int") <|>
        try (s "long") <|>
        try (s "register") <|>
        try (s "restrict") <|>
        try (s "return") <|>
        try (s "short") <|>
        try (s "signed") <|>
        try (s "sizeof") <|>
        try (s "static") <|>
        try (s "struct") <|>
        try (s "switch") <|>
        try (s "typedef") <|>
        try (s "union") <|>
        try (s "unsigned") <|>
        try (s "void") <|>
        try (s "volatile") <|>
        try (s "while") <|>
        try (s "_Alignas") <|>
        try (s "_Alignof") <|>
        try (s "_Atomic") <|>
        try (s "_Bool") <|>
        try (s "_Complex") <|>
        try (s "_Generic") <|>
        try (s "_Imaginary") <|>
        try (s "_No_return") <|>
        try (s "_Static_assert") <|>
        (s "_Thread_local")
        }
    return $ keywordStringToKeyword keyword

keywordStringToKeyword :: String -> Keyword
keywordStringToKeyword keywordString = case keywordString of
    "auto" -> Auto
    "break" -> Break
    "case" -> Case
    "char" -> Char
    "const" -> Const
    "continue" -> Continue
