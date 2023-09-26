module Compiler.CodeGeneration.GenerateAbstractAssembly where
import AbstractSyntaxTree
import Compiler.CodeGeneration.AbstractAssembly
import Compiler.SymbolTable.SymbolTable


generateAbstractAssemblyFromSymbolTable :: SymbolTable -> AbstractAssemblyFile
generateAbstractAssemblyFromSymbolTable (SymbolTable symbolTableEntryList) = AbstractAssemblyFile $ map convertSymbolTableEntryToAssemblyDefinition symbolTableEntryList

convertSymbolTableEntryToAssemblyDefinition :: SymbolTableEntry -> AssemblyDefinition
convertSymbolTableEntryToAssemblyDefinition (SymbolTableEntry symbolName typeSpecifier) =
    AssemblyDefinition symbolName assemblyInstructionOrDirectiveList
    where
        assemblyInstructionOrDirectiveList = convertTypeSpecifierToAssemblyInstructionsOrDirectiveList typeSpecifier

convertTypeSpecifierToAssemblyInstructionsOrDirectiveList :: TypeSpecifier -> [AssemblyInstructionOrDirective]
convertTypeSpecifierToAssemblyInstructionsOrDirectiveList typeSpecifier = case typeSpecifier of
    Int_ -> intDirectiveList
    _ -> error "code generation error: type specifier other than int not supported"
    where
        intDirectiveList = [intDirective]

intDirective :: AssemblyInstructionOrDirective
intDirective = AssemblyDirective $ ReserveBytesDirective 4

