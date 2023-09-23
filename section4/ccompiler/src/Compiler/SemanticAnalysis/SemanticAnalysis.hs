module Compiler.SemanticAnalysis.SemanticAnalysis where
import AbstractSyntaxTree
import Compiler.SymbolTable.SymbolTable
import Data.List.NonEmpty
import Data.Maybe

convertAbstractSyntaxTreeToSymbolTable :: TranslationUnit -> SymbolTable
convertAbstractSyntaxTreeToSymbolTable translationUnit = SymbolTable . toList $ Data.List.NonEmpty.map convertExternalDeclarationToSymbolTableEntry externalDeclarations
    where
        externalDeclarations = extractExternalDeclarationsFromTranslationUnit translationUnit

extractExternalDeclarationsFromTranslationUnit :: TranslationUnit -> NonEmpty ExternalDeclaration
extractExternalDeclarationsFromTranslationUnit (TranslationUnit externalDeclarations) = externalDeclarations

convertExternalDeclarationToSymbolTableEntry :: ExternalDeclaration -> SymbolTableEntry
convertExternalDeclarationToSymbolTableEntry externalDeclaration = case externalDeclaration of
    (FunctionDefinitionExternalDeclaration functionDefinition) -> convertFunctionDefinitionToSymbolTableEntry functionDefinition
    (DeclarationExternalDeclaration declaration) -> convertDeclarationToSymbolTableEntry declaration

convertFunctionDefinitionToSymbolTableEntry :: FunctionDefinition -> SymbolTableEntry
convertFunctionDefinitionToSymbolTableEntry _ = error "semantic error: Function definitions not supported"

convertDeclarationToSymbolTableEntry :: Declaration -> SymbolTableEntry
convertDeclarationToSymbolTableEntry declaration = case declaration of
    DeclarationSpecifiersDeclaration declarationSpecifiers maybeInitDeclaratorList -> SymbolTableEntry (extractSymbolNameFromMaybeInitDeclaratorList maybeInitDeclaratorList) (extractTypeSpecifierFromDeclarationSpecifiers declarationSpecifiers)
    StaticAssertDeclarationDeclaration staticAssertDeclaration -> error "semantic error: static assert declaration not supported"
    where


extractTypeSpecifierFromDeclarationSpecifiers :: DeclarationSpecifiers -> TypeSpecifier
extractTypeSpecifierFromDeclarationSpecifiers (DeclarationSpecifiers nonEmptyListDeclarationSpecifier) = typeSpecifier
    where
        declarationSpecifierList = toList nonEmptyListDeclarationSpecifier
        maybeTypeSpecifierList = mapMaybe declarationSpecifierToMaybeTypeSpecifier declarationSpecifierList
        typeSpecifier = if Prelude.length maybeTypeSpecifierList /= 1 then error "semantic error: one and only one type specifier allowed; your code is not supported" else Prelude.head maybeTypeSpecifierList



--convertDeclarationSpecifierNonEmptyListToMaybeTypeSpecifierNonEmptyList :: NonEmpty DeclarationSpecifier -> NonEmpty (Maybe TypeSpecifier)
--convertDeclarationSpecifierNonEmptyListToMaybeTypeSpecifierNonEmptyList = mapMaybe

declarationSpecifierToMaybeTypeSpecifier :: DeclarationSpecifier -> Maybe TypeSpecifier
declarationSpecifierToMaybeTypeSpecifier declarationSpecifier = case declarationSpecifier of
    TypeSpecifierDeclarationSpecifier typeSpecifier -> Just typeSpecifier
    _ -> Nothing

isTypeSpecifier :: DeclarationSpecifier -> Bool
isTypeSpecifier declarationSpecifier = case declarationSpecifier of
    TypeSpecifierDeclarationSpecifier _ -> True
    _ -> False

extractSymbolNameFromMaybeInitDeclaratorList :: Maybe InitDeclaratorList -> SymbolName
extractSymbolNameFromMaybeInitDeclaratorList maybeInitDeclaratorList = case maybeInitDeclaratorList of
    Just initDeclaratorList -> extractSymbolNameFromInitDeclaratorList initDeclaratorList
    Nothing -> error "semantic error: empty init declarator list not supported yet (like struct or union declarations without declarator for an object"

extractSymbolNameFromInitDeclaratorList :: InitDeclaratorList -> SymbolName
extractSymbolNameFromInitDeclaratorList (InitDeclaratorList initDeclaratorNonEmptyList) =
    extractSymbolNameFromInitDeclarator $ Data.List.NonEmpty.head initDeclaratorNonEmptyList

extractSymbolNameFromInitDeclarator :: InitDeclarator -> SymbolName
extractSymbolNameFromInitDeclarator initDeclarator = case initDeclarator of
    SimpleInitDeclarator declarator -> extractSymbolNameFromDeclarator declarator
    ComplexInitDeclarator declarator _ -> extractSymbolNameFromDeclarator declarator

extractSymbolNameFromDeclarator :: Declarator -> SymbolName
extractSymbolNameFromDeclarator (Declarator maybePointer directDeclarator)
    | Just _ <- maybePointer = error "semantic error: pointers not supported yet"
    | IdentifierDirectDeclarator identifier <- directDeclarator = SymbolName identifier
    | otherwise = error "semantic error: no symbol name found in declarator"

