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


main :: IO Integer
main = do
    putStrLn "C Compiler in development, but here goes..."
    args <- getArgs
    let inputFileName = args !! 0
    putStrLn $ "input file name: \"" ++ inputFileName ++ "\""
    fileContents <- readFile inputFileName
    putStrLn $ "Below are the file contents:\n\n" ++ fileContents
    -- compile the file contents
    let lexedString = lexString fileContents
    let eitherAstOrError = parse translationUnit inputFileName lexedString
    let ast = fromRight (error (show eitherAstOrError)) eitherAstOrError
    let symbolTable = convertAbstractSyntaxTreeToSymbolTable ast
    let abstractAssemblyFile = generateAbstractAssemblyFromSymbolTable symbolTable
    let serializedAssembly = toAssembly abstractAssemblyFile
    -- write the compiled assembly to the output file
    putStrLn $ "I worked very hard to compile your code. Here is the output assembly:\n\n" ++ serializedAssembly
    writeFile outputFileName serializedAssembly
    return 0

outputFileName :: String
outputFileName = "output.S"

