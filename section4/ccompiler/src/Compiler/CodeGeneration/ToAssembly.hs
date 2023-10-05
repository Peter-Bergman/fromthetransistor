{-# LANGUAGE FlexibleInstances #-}

module Compiler.CodeGeneration.ToAssembly where
import AbstractSyntaxTree
import Compiler.CodeGeneration.AbstractAssembly
import Compiler.SymbolTable.SymbolTable
import Data.List
    (intercalate)


class ToAssembly a where
    toAssembly :: a -> String


instance ToAssembly AbstractAssemblyFile where
    toAssembly (AbstractAssemblyFile assemblyDefinitions) = serializeBssSection bssAssemblyDefinitions
        where bssAssemblyDefinitions = assemblyDefinitions -- HACK: in minimal pipeline, all definitions are bss definitions

serializeBssSection :: [AssemblyDefinition] -> String
serializeBssSection assemblyDefinitions = bssSectionDirective ++ "\n" ++ combinedDefinitions
    where
        bssSectionDirective = "\t.section bss"
        serializedDefinitions = map toAssembly assemblyDefinitions
        combinedDefinitions = intercalate "\n\n" serializedDefinitions

instance ToAssembly AssemblyDefinition where
    toAssembly (AssemblyDefinition symbolName assemblyInstructionOrDirectiveList) = allLinesOfDefinition
        where
            allLinesOfDefinition = labelLine ++ instructionOrDirectiveLines
            labelLine = toAssembly symbolName ++ ":"
            instructionOrDirectiveLines = intercalate "\n" instructionOrDirectiveLinesList
            instructionOrDirectiveLinesList = map toAssembly assemblyInstructionOrDirectiveList

instance ToAssembly AssemblyInstructionOrDirective where
    toAssembly assemblyInstructionOrDirective = case assemblyInstructionOrDirective of
        AssemblyInstruction assemblyInstruction -> toAssembly assemblyInstruction
        AssemblyDirective assemblyDirective -> toAssembly assemblyDirective

instance ToAssembly AssemblyDirective where
    toAssembly assemblyDirective = case assemblyDirective of
        ReserveBytesDirective numberOfBytesToReserve -> "\t.bytes " ++ show numberOfBytesToReserve

instance ToAssembly AssemblyInstruction where
    toAssembly assemblyInstruction = case assemblyInstruction of
        AssemblyInstructionNotImplementedYet -> error "serialization error: assembly instructions not implemented yet"

instance ToAssembly SymbolName where
    toAssembly (SymbolName identifier) = toAssembly identifier

instance ToAssembly Identifier where
    toAssembly (Identifier identifierNonDigit identifierSuffixList) = toAssembly identifierNonDigit ++ toAssembly identifierSuffixList

instance ToAssembly IdentifierNonDigit where
    toAssembly (NonDigitIdentifierNonDigit nonDigit) = toAssembly nonDigit
    toAssembly (UniversalCharacterNameIdentifierNonDigit universalCharacterName) = toAssembly universalCharacterName

instance ToAssembly NonDigit where
    toAssembly (NonDigit character) = character : ""

instance ToAssembly UniversalCharacterName where
    toAssembly _ = error "serialization error: universal character names not implemented yet"

instance ToAssembly [IdentifierSuffix] where
    toAssembly identifierSuffixList = intercalate "" $ listOfSerializedIdentifierSuffixes
        where
            listOfSerializedIdentifierSuffixes = map toAssembly identifierSuffixList

instance ToAssembly IdentifierSuffix where
    toAssembly (IdentifierNonDigitIdentifierSuffix identifierNonDigit) = toAssembly identifierNonDigit
    toAssembly (DigitIdentifierSuffix digit) = toAssembly digit

instance ToAssembly Digit where
    toAssembly _ = error "serialization error: digits not implemented yet"

