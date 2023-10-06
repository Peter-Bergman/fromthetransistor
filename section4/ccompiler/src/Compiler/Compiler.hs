module Compiler.Compiler where
import Compiler.CodeGeneration.GenerateAbstractAssembly
import Compiler.CodeGeneration.AbstractAssembly
import Compiler.CodeGeneration.ToAssembly
import Compiler.Parser.TranslationUnit
import Compiler.SemanticAnalysis.SemanticAnalysis
import Compiler.SymbolTable.SymbolTable
import Data.Either
import Lexer.Lexer
import Text.Parsec.Prim


compile :: String -> String -> String
compile inputTranslationUnit inputFileName = serializedAssembly
    where
        lexedString = lexString inputTranslationUnit
        eitherAstOrError = parse translationUnit inputFileName lexedString
        ast = fromRight (error (show eitherAstOrError)) eitherAstOrError
        symbolTable = convertAbstractSyntaxTreeToSymbolTable ast
        abstractAssemblyFile = generateAbstractAssemblyFromSymbolTable symbolTable
        serializedAssembly = toAssembly abstractAssemblyFile

