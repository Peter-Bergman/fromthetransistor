module Compiler.Parser.TranslationUnit where
import Preprocessor.ConstantExpression
import Preprocessor.Identifier
import Preprocessor.PreprocessingParser
import AbstractSyntaxTree
import CustomCombinators
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Prim

type CompilerParserX = PreprocessingParserX

translationUnit :: CompilerParserX TranslationUnit
translationUnit = simpleExpression (many1NonEmpty externalDeclaration) TranslationUnit

externalDeclaration :: CompilerParserX ExternalDeclaration
externalDeclaration = functionDefinitionExternalDeclaration <|> declarationExternalDeclaration

functionDefinitionExternalDeclaration :: CompilerParserX ExternalDeclaration
functionDefinitionExternalDeclaration = simpleExpression functionDefinition FunctionDefinitionExternalDeclaration

functionDefinition :: CompilerParserX FunctionDefinition
functionDefinition = parserFail "functionDefinition not implemented"

declarationExternalDeclaration :: CompilerParserX ExternalDeclaration
declarationExternalDeclaration = simpleExpression declaration DeclarationExternalDeclaration

declaration :: CompilerParserX Declaration
declaration = declarationSpecifiersDeclaration <|> staticAssertDeclarationDeclaration

staticAssertDeclarationDeclaration :: CompilerParserX Declaration
staticAssertDeclarationDeclaration = parserFail "not implemented"

declarationSpecifiersDeclaration :: CompilerParserX Declaration
declarationSpecifiersDeclaration = try $ do
    parsedDeclarationSpecifiers <- declarationSpecifiers
    parsedMaybeInitDeclaratorList <- tryMaybe initDeclaratorList
    _ <- semicolon
    return $ DeclarationSpecifiersDeclaration parsedDeclarationSpecifiers parsedMaybeInitDeclaratorList

declarationSpecifiers :: CompilerParserX DeclarationSpecifiers
declarationSpecifiers = tryWithFailMessage "declaration specifiers" $ simpleExpression (many1NonEmpty declarationSpecifier) DeclarationSpecifiers

declarationSpecifier :: CompilerParserX DeclarationSpecifier
declarationSpecifier =
    storageClassDeclarationSpecifier <|>
    typeSpecifierDeclarationSpecifier <|>
    typeQualifierDeclarationSpecifier <|>
    functionSpecifierDeclarationSpecifier <|>
    alignmentDeclarationSpecifier

storageClassDeclarationSpecifier :: CompilerParserX DeclarationSpecifier
storageClassDeclarationSpecifier = parserFail "not implemented"

typeSpecifierDeclarationSpecifier :: CompilerParserX DeclarationSpecifier
typeSpecifierDeclarationSpecifier = simpleExpression typeSpecifier TypeSpecifierDeclarationSpecifier

typeQualifierDeclarationSpecifier :: CompilerParserX DeclarationSpecifier
typeQualifierDeclarationSpecifier = parserFail "not implemented"

functionSpecifierDeclarationSpecifier :: CompilerParserX DeclarationSpecifier
functionSpecifierDeclarationSpecifier = parserFail "not implemented"

alignmentDeclarationSpecifier :: CompilerParserX DeclarationSpecifier
alignmentDeclarationSpecifier = parserFail "not implemented"

semicolon :: CompilerParserX ()
semicolon = stringSatisfy_ (==";")

initDeclaratorList :: CompilerParserX InitDeclaratorList
initDeclaratorList = tryWithFailMessage "init declarator list" $ do
    parsedInitDeclarators <- sepBy1NonConsumptionNonEmpty initDeclarator comma
    return $ InitDeclaratorList parsedInitDeclarators

initDeclarator :: CompilerParserX InitDeclarator
initDeclarator = complexInitDeclarator <|> simpleInitDeclarator <?> "init declarator"

simpleInitDeclarator :: CompilerParserX InitDeclarator
simpleInitDeclarator = simpleExpression declarator SimpleInitDeclarator

complexInitDeclarator :: CompilerParserX InitDeclarator
complexInitDeclarator = try $ do
    parsedDeclarator <- declarator
    _ <- equalSign
    parsedInitializer <- initializer
    return $ ComplexInitDeclarator parsedDeclarator parsedInitializer

{-
initializer :: CompilerParserX Initializer
initializer = assignmentExpressionInitializer <|> initializerListInitializer

assignmentExpressionInitializer :: CompilerParserX Initializer
assignmentExpressionInitializer = simpleExpression assignmentExpression AssignmentExpressionInitializer

initializerListInitializer = parserFail "not implemented"
-}

typeSpecifier :: CompilerParserX TypeSpecifier
typeSpecifier = tryWithFailMessage "Type Specifier" $ anyOf
    [ stringSatisfy_ (=="void") >> return Void
    , stringSatisfy_ (=="char") >> return Char_
    , stringSatisfy_ (=="short") >> return Short
    , stringSatisfy_ (=="int") >> return Int_
    , stringSatisfy_ (=="long") >> return Long
    , stringSatisfy_ (=="float") >> return Float_
    , stringSatisfy_ (=="double") >> return Double_
    , stringSatisfy_ (=="signed") >> return Signed
    , stringSatisfy_ (=="unsigned") >> return Unsigned
    , stringSatisfy_ (=="_Bool") >> return Bool_
    , stringSatisfy_ (=="_Complex") >> return Complex_
    , atomicTypeSpecifierTypeSpecifier
    , structOrUnionSpecifierTypeSpecifier
    , enumSpecifierTypeSpecifier
    , typeDefNameTypeSpecifier
    ]

atomicTypeSpecifierTypeSpecifier :: CompilerParserX TypeSpecifier
atomicTypeSpecifierTypeSpecifier = parserFail "not implemented"

structOrUnionSpecifierTypeSpecifier :: CompilerParserX TypeSpecifier
structOrUnionSpecifierTypeSpecifier = parserFail "not implemented"

enumSpecifierTypeSpecifier :: CompilerParserX TypeSpecifier
enumSpecifierTypeSpecifier = parserFail "not implemented"

typeDefNameTypeSpecifier :: CompilerParserX TypeSpecifier
typeDefNameTypeSpecifier = parserFail "not implemented"

assignmentExpression :: CompilerParserX AssignmentExpression
assignmentExpression = complexAssignmentExpression <|> simpleAssignmentExpression

simpleAssignmentExpression :: CompilerParserX AssignmentExpression
simpleAssignmentExpression = simpleExpression conditionalExpression SimpleAssignmentExpression

complexAssignmentExpression :: CompilerParserX AssignmentExpression
complexAssignmentExpression = parserFail "complex assignment expression not implemented"

declarator :: CompilerParserX Declarator
declarator = tryWithFailMessage "Declarator" $ do
    parsedMaybePointer <- tryMaybe pointer
    parsedDirectDeclarator <- directDeclarator
    return $ Declarator parsedMaybePointer parsedDirectDeclarator

pointer :: CompilerParserX Pointer
pointer = parserFail "pointer not implemented"

directDeclarator :: CompilerParserX DirectDeclarator
directDeclarator =
    identifierDirectDeclarator <|>
    simpleDirectDeclarator <|>
    maybeAssignmentDirectDeclarator <|>
    staticFirstDirectDeclarator <|>
    typeQualifierListDirectDeclarator <|>
    asteriskDirectDeclarator <|>
    parameterListDirectDeclarator <|>
    identifierListDirectDeclarator

identifierDirectDeclarator :: CompilerParserX DirectDeclarator
identifierDirectDeclarator = simpleExpression identifier IdentifierDirectDeclarator

simpleDirectDeclarator :: CompilerParserX DirectDeclarator
simpleDirectDeclarator = try $ parens directDeclarator

maybeAssignmentDirectDeclarator :: CompilerParserX DirectDeclarator
maybeAssignmentDirectDeclarator = parserFail "maybeAssignmentDirectDeclarator not implemented"

staticFirstDirectDeclarator :: CompilerParserX DirectDeclarator
staticFirstDirectDeclarator = parserFail "staticFirstDirectDeclarator not implemented"

typeQualifierListDirectDeclarator :: CompilerParserX DirectDeclarator
typeQualifierListDirectDeclarator = parserFail "typeQualifierListDirectDeclarator not implemented"

asteriskDirectDeclarator :: CompilerParserX DirectDeclarator
asteriskDirectDeclarator = parserFail "asteriskDirectDeclarator not implemented"

parameterListDirectDeclarator :: CompilerParserX DirectDeclarator
parameterListDirectDeclarator = parserFail "parameterListDirectDeclarator not implemented"

identifierListDirectDeclarator :: CompilerParserX DirectDeclarator
identifierListDirectDeclarator = parserFail "identifierListDirectDeclarator not implemented"

