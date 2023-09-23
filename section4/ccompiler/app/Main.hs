module Main (main) where
import AbstractSyntaxTree
import Compiler.Parser.TranslationUnit
import Compiler.SemanticAnalysis.SemanticAnalysis
import Compiler.SymbolTable.SymbolTable
import Data.Either
import Lexer.Lexer
import System.Environment
import Text.Parsec.Prim
import Text.Parsec.Error

--import Lib

main :: IO ()
main = putStrLn "C Compiler in development..."

lexedString :: [String]
lexedString = lexString "int x;\n"

eitherAstOrError :: Either ParseError TranslationUnit
eitherAstOrError = parse translationUnit "" lexedString

-- unsafe
ast :: TranslationUnit
ast = fromRight (error (show eitherAstOrError)) eitherAstOrError

symbolTable :: SymbolTable
symbolTable = convertAbstractSyntaxTreeToSymbolTable ast

