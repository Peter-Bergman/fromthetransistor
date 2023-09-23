module Compiler.SymbolTable.SymbolTable where
import AbstractSyntaxTree

newtype SymbolTable = SymbolTable [SymbolTableEntry]
    deriving (Show)

data SymbolTableEntry = SymbolTableEntry SymbolName TypeSpecifier
    deriving (Show)

newtype SymbolName = SymbolName Identifier
    deriving (Show)

