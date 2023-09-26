{-# LANGUAGE DeriveGeneric #-}
module Compiler.CodeGeneration.AbstractAssembly where
import Compiler.SymbolTable.SymbolTable
    (SymbolName)
import GHC.Generics

newtype AbstractAssemblyFile =
    AbstractAssemblyFile [AssemblyDefinition]
    deriving (Show, Generic)

data AssemblyDefinition =
    AssemblyDefinition SymbolName [AssemblyInstructionOrDirective]
    deriving (Show, Generic)

data AssemblyInstructionOrDirective =
    AssemblyInstruction AssemblyInstruction |
    AssemblyDirective AssemblyDirective
    deriving (Show, Generic)

-- TODO: implement
data AssemblyInstruction =
    AssemblyInstructionNotImplementedYet
    deriving (Show, Generic)

data AssemblyDirective =
    ReserveBytesDirective Integer
    deriving (Show, Generic)

