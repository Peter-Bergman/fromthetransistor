module Main (main) where
import AbstractSyntaxTree
import Compiler.CodeGeneration.GenerateAbstractAssembly
import Compiler.CodeGeneration.AbstractAssembly
import Compiler.CodeGeneration.ToAssembly
import Compiler.Parser.TranslationUnit
import Compiler.SemanticAnalysis.SemanticAnalysis
import Compiler.SymbolTable.SymbolTable
import Data.Either
import Lexer.Lexer
import System.Environment
import Text.Parsec.Prim
import Text.Parsec.Error

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

abstractAssemblyFile :: AbstractAssemblyFile
abstractAssemblyFile = generateAbstractAssemblyFromSymbolTable symbolTable

serializedAssembly :: String
serializedAssembly = toAssembly abstractAssemblyFile

