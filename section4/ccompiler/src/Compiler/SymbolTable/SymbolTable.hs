module Compiler.SymbolTable.SymbolTable where
import AbstractSyntaxTree

newtype SymbolTable = SymbolTable [SymbolTableEntry]

data SymbolTableEntry = SymbolTableEntry SymbolName TypeSpecifier

newtype SymbolName = SymbolName Identifier

