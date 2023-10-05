module Preprocessor.ConstantExpression where
import AbstractSyntaxTree
import CustomCombinators
import Data.List.NonEmpty
import Preprocessor.Constant
import Preprocessor.Identifier
import Preprocessor.PreprocessingParser
import Preprocessor.StringLiteral
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Char
import Text.Parsec.Char


parens :: PreprocessingParserX t -> PreprocessingParserX t
parens = between leftParenthesis rightParenthesis

brackets :: PreprocessingParserX t -> PreprocessingParserX t
brackets = between leftBracket rightBracket

braces :: PreprocessingParserX t -> PreprocessingParserX t
braces = between leftBrace rightBrace

leftParenthesis :: PreprocessingParserX ()
leftParenthesis = stringSatisfy_ (=="(")

rightParenthesis :: PreprocessingParserX ()
rightParenthesis = stringSatisfy_ (==")")

leftBracket :: PreprocessingParserX ()
leftBracket = stringSatisfy_ (=="[")

rightBracket :: PreprocessingParserX ()
rightBracket = stringSatisfy_ (=="]")

leftBrace :: PreprocessingParserX ()
leftBrace = stringSatisfy_ (=="{")

rightBrace :: PreprocessingParserX ()
rightBrace = stringSatisfy_ (=="}")

infixRecursiveBinaryOperatorExpression :: PreprocessingParserX returnType -> PreprocessingParserX primitiveType -> PreprocessingParserX operator -> (returnType -> primitiveType -> returnType) -> PreprocessingParserX returnType
infixRecursiveBinaryOperatorExpression recursedParser primitiveParser operatorParser typeConstructor = try $ do
    parsedPrimitiveType <- primitiveParser
    _ <- operatorParser
    parsedReturnType <- recursedParser
    return $ typeConstructor parsedReturnType parsedPrimitiveType

-- NOTE: maybe onlyInConstantExpressions could be used in the future; could maybe read parser state
{-onlyInConstantExpressions ::  PreprocessingParserX t -> PreprocessingParserX t
onlyInConstantExpressions isPartOfConstantExpression parser =
    if isPartOfConstantExpression then parser else parserFail "defined unary operator can only be part of constant expressions"
-}

constantExpression :: PreprocessingParserX ConstantExpression
constantExpression = simpleExpression conditionalExpression ConstantExpression

conditionalExpression :: PreprocessingParserX ConditionalExpression
conditionalExpression = simpleConditionalExpression <|> complexConditionalExpression

simpleConditionalExpression :: PreprocessingParserX ConditionalExpression
simpleConditionalExpression = simpleExpression logicalOrExpression SimpleConditionalExpression

complexConditionalExpression :: PreprocessingParserX ConditionalExpression
complexConditionalExpression = (parserFail "complexConditionalExpression not implemented") {->> do
    parsedLogicalOrExpression <- logicalOrExpression
    questionMark
    parsedExpression <- expression
    colon
    parsedConditionalExpression <- conditionalExpression
    return $ ComplexConditionalExpression parsedLogicalOrExpression parsedExpression parsedConditionalExpression
-}

questionMark :: PreprocessingParserX ()
questionMark = stringSatisfy_ (=="?")

colon :: PreprocessingParserX ()
colon = stringSatisfy_ (==":")

logicalOrExpression :: PreprocessingParserX LogicalOrExpression
logicalOrExpression = try $ sepBy1 logicalAndExpression logicalOrOperator >>= return . LogicalOrExpression . fromList

logicalOrOperator :: PreprocessingParserX ()
logicalOrOperator = stringSatisfy_ (=="||")

logicalAndExpression ::  PreprocessingParserX LogicalAndExpression
logicalAndExpression = try $ sepBy1 inclusiveOrExpression logicalAndOperator >>= return . LogicalAndExpression . fromList

logicalAndOperator :: PreprocessingParserX ()
logicalAndOperator = stringSatisfy_ (=="&&")

inclusiveOrExpression ::  PreprocessingParserX InclusiveOrExpression
inclusiveOrExpression = try $ sepBy1 exclusiveOrExpression bitwiseOrOperator >>= return . InclusiveOrExpression . fromList

bitwiseOrOperator :: PreprocessingParserX ()
bitwiseOrOperator = stringSatisfy_ (=="|")

exclusiveOrExpression ::  PreprocessingParserX ExclusiveOrExpression
exclusiveOrExpression = try $ sepBy1 andExpression bitwiseXorOperator >>= return . ExclusiveOrExpression . fromList

bitwiseXorOperator :: PreprocessingParserX ()
bitwiseXorOperator = stringSatisfy_ (=="^")

andExpression ::  PreprocessingParserX AndExpression
andExpression = try $ sepBy1 equalityExpression bitwiseAndOperator >>= return . AndExpression . fromList

bitwiseAndOperator :: PreprocessingParserX ()
bitwiseAndOperator = stringSatisfy_ (=="&")


equalityExpression :: PreprocessingParserX EqualityExpression
equalityExpression =
    try equalityOperatorExpression <|>
    try inequalityExpression <|>
    simpleEqualityExpression
    --(relationalExpression >>= return . RelationalExpression)

equalityOperatorExpression :: PreprocessingParserX EqualityExpression
equalityOperatorExpression = (parserFail "equalityOperatorExpression not implemented") {->> do
    parsedRelationalExpression <- relationalExpression
    equalityOperator
    parsedEqualityExpression <- equalityExpression
    return $ EqualityExpression parsedEqualityExpression parsedRelationalExpression
-}

equalityOperator :: PreprocessingParserX ()
equalityOperator = stringSatisfy_ (=="==")

inequalityExpression :: PreprocessingParserX EqualityExpression
inequalityExpression = (parserFail "inequalityExpression not implemented") {->> do
    parsedRelationalExpression <- relationalExpression
    inequalityOperator
    parsedEqualityExpression <- equalityExpression
    return $ InequalityExpression parsedEqualityExpression parsedRelationalExpression
-}

inequalityOperator :: PreprocessingParserX ()
inequalityOperator = stringSatisfy_ (=="!=")

simpleEqualityExpression :: PreprocessingParserX EqualityExpression
simpleEqualityExpression = simpleExpression relationalExpression SimpleEqualityExpression

relationalExpression :: PreprocessingParserX RelationalExpression
relationalExpression =
    lessThanExpression <|>
    greaterThanExpression <|>
    lessThanOrEqualToExpression <|>
    greaterThanOrEqualToExpression <|>
    simpleRelationalExpression

lessThanExpression :: PreprocessingParserX RelationalExpression
lessThanExpression = (parserFail "not implemented or implementation not used") {->> try $ infixRecursiveBinaryOperatorExpression relationalExpression shiftExpression lessThanOperator LessThanExpression
-}

{-lessThanExpression = do
    parsedShiftExpression <- shiftExpression
    lessThanOperator
    parsedRelationalExpression <- relationalExpression
    return $ LessThanExpression parsedRelationalExpression parsedShiftExpression
-}

lessThanOperator :: PreprocessingParserX ()
lessThanOperator = stringSatisfy_ (=="<")

greaterThanExpression :: PreprocessingParserX RelationalExpression
greaterThanExpression = (parserFail "not implemented or implementation not used") {->> try $ infixRecursiveBinaryOperatorExpression relationalExpression shiftExpression greaterThanOperator GreaterThanExpression
-}

{-greaterThanExpression = do
    parsedShiftExpression <- shiftExpression
    greaterThanOperator
    parsedRelationalExpression <- relationalExpression
    return $ GreaterThanExpression parsedRelationalExpression parsedShiftExpression-}

greaterThanOperator :: PreprocessingParserX ()
greaterThanOperator = stringSatisfy_ (==">")

lessThanOrEqualToExpression :: PreprocessingParserX RelationalExpression
lessThanOrEqualToExpression = (parserFail "not implemented or implementation not used") {->> try $ infixRecursiveBinaryOperatorExpression relationalExpression shiftExpression lessThanOrEqualToOperator LessThanOrEqualToExpression
{-lessThanOrEqualToExpression = do
    parsedShiftExpression <- shiftExpression
    lessThanOrEqualToOperator
    parsedRelationalExpression <- relationalExpression-}
-}

lessThanOrEqualToOperator :: PreprocessingParserX ()
lessThanOrEqualToOperator = stringSatisfy_ (=="<=")

greaterThanOrEqualToExpression :: PreprocessingParserX RelationalExpression
greaterThanOrEqualToExpression = (parserFail "not implemented or implementation not used") {->> try $ infixRecursiveBinaryOperatorExpression relationalExpression shiftExpression greaterThanOrEqualToExpression GreaterThanOrEqualToExpression
{-greaterThanOrEqualToExpression = do
    parsedShiftExpression <- shiftExpression
    greaterThanOrEqualToOperator
    parsedRelationalExpression <- relationalExpression
-}
-}

greaterThanOrEqualToOperator :: PreprocessingParserX ()
greaterThanOrEqualToOperator = stringSatisfy_ (==">=")

simpleRelationalExpression :: PreprocessingParserX RelationalExpression
simpleRelationalExpression = simpleExpression shiftExpression SimpleRelationalExpression
--simpleRelationalExpression = try $ shiftExpression >>= return . SimpleRelationalExpression

shiftExpression :: PreprocessingParserX ShiftExpression
shiftExpression =
    leftShiftExpression <|>
    rightShiftExpression <|>
    simpleShiftExpression

leftShiftExpression :: PreprocessingParserX ShiftExpression
leftShiftExpression = (parserFail "not implemented or implementation not used") {->> try $ infixRecursiveBinaryOperatorExpression shiftExpression additiveExpression leftShiftOperator LeftShiftExpression
{-leftShiftExpression = do
    parsedAdditiveExpression <- additiveExpression
    leftShiftOperator
    parsedShiftExpression <- shiftExpression
    return $ LeftShiftExpression parsedShiftExpression parsedAdditiveExpression
-}
-}

leftShiftOperator :: PreprocessingParserX ()
leftShiftOperator = stringSatisfy_ (=="<<")

rightShiftExpression :: PreprocessingParserX ShiftExpression
rightShiftExpression = (parserFail "not implemented or implementation not used") {->> try $ infixRecursiveBinaryOperatorExpression shiftExpression additiveExpression rightShiftOperator RightShiftExpression
{-rightShiftExpression = do
    parsedAdditiveExpression <- additiveExpression
    rightShiftOperator
    parsedShiftExpression <- shiftExpression
    return $ RightShiftExpression parsedShiftExpression parsedAdditiveExpression
-}
-}

rightShiftOperator :: PreprocessingParserX ()
rightShiftOperator = stringSatisfy_ (==">>")

simpleShiftExpression :: PreprocessingParserX ShiftExpression
simpleShiftExpression = simpleExpression additiveExpression SimpleShiftExpression
--simpleShiftExpression = try $ additiveExpression >>= return . SimpleShiftExpression

additiveExpression :: PreprocessingParserX AdditiveExpression
additiveExpression =
    additionExpression  <|>
    subtractionExpression <|>
    simpleAdditiveExpression

additionExpression :: PreprocessingParserX AdditiveExpression
additionExpression = (parserFail "not implemented or implementation not used") {->> infixRecursiveBinaryOperatorExpression additiveExpression multiplicativeExpression additionOperator AdditionExpression
-}

additionOperator :: PreprocessingParserX ()
additionOperator = stringSatisfy_ (=="+")

subtractionExpression :: PreprocessingParserX AdditiveExpression
subtractionExpression = (parserFail "not implemented or implementation not used") {->> infixRecursiveBinaryOperatorExpression additiveExpression multiplicativeExpression subtractionOperator SubtractionExpression
-}

subtractionOperator :: PreprocessingParserX ()
subtractionOperator = stringSatisfy_ (=="-")

simpleAdditiveExpression :: PreprocessingParserX AdditiveExpression
simpleAdditiveExpression = multiplicativeExpression >>= return . SimpleAdditiveExpression

multiplicativeExpression :: PreprocessingParserX MultiplicativeExpression
multiplicativeExpression =
    multiplicationExpression <|>
    divisionExpression <|>
    moduloExpression <|>
    simpleMultiplicativeExpression

multiplicationExpression :: PreprocessingParserX MultiplicativeExpression
multiplicationExpression = (parserFail "not implemented or implementation not used") {->> try $ infixRecursiveBinaryOperatorExpression multiplicativeExpression castExpression multiplicationOperator MultiplicationExpression
-}

multiplicationOperator :: PreprocessingParserX ()
multiplicationOperator = asterisk

asterisk :: PreprocessingParserX ()
asterisk = stringSatisfy_ (=="*")

divisionExpression :: PreprocessingParserX MultiplicativeExpression
divisionExpression = (parserFail "not implemented or implementation not used") {->> try $ infixRecursiveBinaryOperatorExpression multiplicativeExpression castExpression divisionOperator DivisionExpression
-}

divisionOperator :: PreprocessingParserX ()
divisionOperator = slash

slash :: PreprocessingParserX ()
slash = stringSatisfy_ (=="/")

moduloExpression :: PreprocessingParserX MultiplicativeExpression
moduloExpression = (parserFail "not implemented or implementation not used") {->> try $ infixRecursiveBinaryOperatorExpression multiplicativeExpression castExpression moduloOperator ModuloExpression
-}

moduloOperator :: PreprocessingParserX ()
moduloOperator = percentSign

percentSign :: PreprocessingParserX ()
percentSign = stringSatisfy_ (=="%")

simpleMultiplicativeExpression :: PreprocessingParserX MultiplicativeExpression
simpleMultiplicativeExpression = simpleExpression castExpression SimpleMultiplicativeExpression

castExpression :: PreprocessingParserX CastExpression
castExpression =
    complexCastExpression <|>
    simpleCastExpression

complexCastExpression :: PreprocessingParserX CastExpression
complexCastExpression = (parserFail "not implemented or implementation not used") {->> do
    parsedTypeName <- parens typeName
    parsedCastExpression <- castExpression
    return $ ComplexCastExpression parsedTypeName parsedCastExpression
-}

simpleCastExpression :: PreprocessingParserX CastExpression
simpleCastExpression = simpleExpression unaryExpression SimpleCastExpression

unaryExpression :: PreprocessingParserX UnaryExpression
unaryExpression = -- double check the ordering of these alternatives
    incrementExpression <|>
    decrementExpression <|>
    sizeOfType <|>
    alignOfType <|>
    definedWithParentheses <|>
    operatorExpression <|>
    sizeOfExpression <|>
    postfixExpressionUnaryExpression

incrementExpression :: PreprocessingParserX UnaryExpression
incrementExpression = (parserFail "complexConditionalExpression not implemented") {->> try $ simpleExpression (incrementOperator >> unaryExpression) IncrementExpression
-}

incrementOperator :: PreprocessingParserX ()
incrementOperator = stringSatisfy_ (=="++")

decrementExpression :: PreprocessingParserX UnaryExpression
decrementExpression = (parserFail "complexConditionalExpression not implemented") {->> try $ simpleExpression (decrementOperator >> unaryExpression) DecrementExpression
-}

decrementOperator :: PreprocessingParserX ()
decrementOperator = stringSatisfy_ (=="--")

sizeOfType :: PreprocessingParserX UnaryExpression
sizeOfType = (parserFail "complexConditionalExpression not implemented") {->> try $ simpleExpression (sizeOf >> parens typeName) SizeOfType
-}

sizeOf :: PreprocessingParserX ()
sizeOf = stringSatisfy_ (=="sizeof")

alignOfType :: PreprocessingParserX UnaryExpression
alignOfType = (parserFail "complexConditionalExpression not implemented") {->> try $ simpleExpression (alignOf >> parens typeName) AlignOfType
-}

alignOf :: PreprocessingParserX ()
alignOf = stringSatisfy_ (=="_Alignof")

definedWithParentheses :: PreprocessingParserX UnaryExpression
definedWithParentheses = (parserFail "complexConditionalExpression not implemented") {->> try $ onlyInConstantExpressions $ simpleExpression (defined >> parens identifier) DefinedWithParentheses
-}

defined :: PreprocessingParserX ()
defined = stringSatisfy_ (=="defined")

operatorExpression :: PreprocessingParserX UnaryExpression
operatorExpression = (parserFail "complexConditionalExpression not implemented") {->> try $ do
    parsedUnaryOperator <- unaryOperator
    parsedCastExpression <- castExpression
    return $ OperatorExpression parsedUnaryOperator parsedCastExpression
-}

unaryOperator :: PreprocessingParserX UnaryOperator
unaryOperator =
    addressOf <|>
    indirection <|>
    plusSign <|>
    minusSign <|>
    bitwiseNot <|>
    logicalNot <|>
    definedWithoutParentheses

addressOf :: PreprocessingParserX UnaryOperator
addressOf = ampersand >> return AddressOf

ampersand :: PreprocessingParserX ()
ampersand = stringSatisfy_ (=="&")

indirection :: PreprocessingParserX UnaryOperator
indirection = asterisk >> return Indirection

plusSign :: PreprocessingParserX UnaryOperator
plusSign = stringSatisfy_ (=="+") >> return PlusSignOperator

minusSign :: PreprocessingParserX UnaryOperator
minusSign = hyphen >> return MinusSignOperator

hyphen :: PreprocessingParserX ()
hyphen = stringSatisfy_ (=="-")

bitwiseNot :: PreprocessingParserX UnaryOperator
bitwiseNot = tilde >> return BitwiseNot

tilde :: PreprocessingParserX ()
tilde = stringSatisfy_ (=="~")

logicalNot :: PreprocessingParserX UnaryOperator
logicalNot = exclamationPoint >> return LogicalNot

exclamationPoint :: PreprocessingParserX ()
exclamationPoint = stringSatisfy_ (=="!")

definedWithoutParentheses :: PreprocessingParserX UnaryOperator
definedWithoutParentheses = parserFail "definedWithoutParentheses not implemented" --onlyInConstantExpressions $ defined >> DefinedWithoutParentheses

sizeOfExpression :: PreprocessingParserX UnaryExpression
sizeOfExpression = (parserFail "complexConditionalExpression not implemented") {->> simpleExpression (sizeOf >> unaryExpression) SizeOfExpression
-}

postfixExpressionUnaryExpression :: PreprocessingParserX UnaryExpression
postfixExpressionUnaryExpression = simpleExpression postfixExpression PostfixExpressionUnaryExpression

postfixExpression :: PreprocessingParserX PostfixExpression
postfixExpression = do
    parsedPrimitivePostfixExpression <- primitivePostfixExpression
    fullPostfixExpression parsedPrimitivePostfixExpression

    {-indexExpression <|>
    functionCallExpression <|>
    dotSelectorExpression <|>
    arrowSelectorExpression <|>
    postfixIncrementExpression <|>
    postfixDecrementExpression <|>
    postfixInitializerListExpression <|>
    simplePostfixExpression-}

primitivePostfixExpression :: PreprocessingParserX PostfixExpression
primitivePostfixExpression = postfixInitializerListExpression <|> simplePostfixExpression

postfixInitializerListExpression :: PreprocessingParserX PostfixExpression
postfixInitializerListExpression = do
    parsedTypeName <- parens typeName
    parsedInitializerList <- braces initializerListWithOptionalFollowingComma
    return $ PostfixInitializerListExpression parsedTypeName parsedInitializerList

simplePostfixExpression :: PreprocessingParserX PostfixExpression
simplePostfixExpression = simpleExpression primaryExpression SimplePostfixExpression

initializerListWithOptionalFollowingComma :: PreprocessingParserX InitializerList
initializerListWithOptionalFollowingComma = do
    parsedInitializerList <- initializerList
    _ <- tryMaybe comma
    return $ parsedInitializerList

initializerList :: PreprocessingParserX InitializerList
initializerList = sepBy1NonConsumption initializerListElement comma >>= return . InitializerList . fromList

initializerListElement :: PreprocessingParserX (Maybe Designation, Initializer)
initializerListElement = do
    parsedMaybeDesignation <- tryMaybe designation
    parsedInitializer <- initializer
    return (parsedMaybeDesignation, parsedInitializer)

comma :: PreprocessingParserX ()
comma = stringSatisfy_ (==",")

designation :: PreprocessingParserX Designation
designation = do
    parsedDesignatorList <- designatorList
    equalSign
    return $ Designation parsedDesignatorList

equalSign :: PreprocessingParserX ()
equalSign = stringSatisfy_ (=="=")

designatorList :: PreprocessingParserX DesignatorList
designatorList = simpleExpression (many1NonEmpty designator) DesignatorList

designator :: PreprocessingParserX Designator
designator =
    bracketDesignator <|>
    dotDesignator

bracketDesignator :: PreprocessingParserX Designator
bracketDesignator = try $ simpleExpression (brackets constantExpression) BracketDesignator

dotDesignator :: PreprocessingParserX Designator
dotDesignator = simpleExpression (dot >> identifier) DotDesignator

dot :: PreprocessingParserX ()
dot = stringSatisfy_ (==".")

initializer :: PreprocessingParserX Initializer
initializer = assignmentExpressionInitializer <|> initializerListInitializer

assignmentExpressionInitializer :: PreprocessingParserX Initializer
assignmentExpressionInitializer = (parserFail "complexConditionalExpression not implemented") {->> simpleExpression assignmentExpression AssignmentExpressionInitializer
-}

initializerListInitializer :: PreprocessingParserX Initializer
initializerListInitializer = simpleExpression initializerListWithOptionalFollowingComma InitializerListInitializer

fullPostfixExpression :: PostfixExpression -> PreprocessingParserX PostfixExpression
fullPostfixExpression inputPrimitivePostfixExpression =
    indexExpression inputPrimitivePostfixExpression <|>
    functionCallExpression inputPrimitivePostfixExpression <|>
    dotSelectorExpression inputPrimitivePostfixExpression <|>
    arrowSelectorExpression inputPrimitivePostfixExpression <|>
    postfixIncrementExpression inputPrimitivePostfixExpression <|>
    postfixDecrementExpression inputPrimitivePostfixExpression <|>
    return inputPrimitivePostfixExpression

indexExpression :: PostfixExpression -> PreprocessingParserX PostfixExpression
indexExpression prefixPostfixExpression = (parserFail "complexConditionalExpression not implemented") {->> do
    parsedIndexExpression <- try $ brackets expression
    return $ IndexExpression prefixPostfixExpression parsedIndexExpression
-}

functionCallExpression :: PostfixExpression -> PreprocessingParserX PostfixExpression
functionCallExpression prefixPostfixExpression = (parserFail "complexConditionalExpression not implemented") {->> do
    parsedMaybeArgumentExpressionList <- try $ parens $ tryMaybe argumentExpressionList
    let parsedFunctionCallExpression = FunctionCallExpression prefixPostfixExpression parsedMaybeArgumentExpressionList
    fullPostfixExpression parsedFunctionCallExpression
-}

dotSelectorExpression :: PostfixExpression -> PreprocessingParserX PostfixExpression
dotSelectorExpression prefixPostfixExpression = (parserFail "complexConditionalExpression not implemented") {->> do
    dot
    parsedIdentifier <- identifier
    return $ DotSelectorExpression prefixPostfixExpression parsedIdentifier
-}

arrowSelectorExpression :: PostfixExpression -> PreprocessingParserX PostfixExpression
arrowSelectorExpression prefixPostfixExpression = (parserFail "complexConditionalExpression not implemented") {->> do
    arrowSelector
    parsedIdentifier <- identifier
    return $ ArrowSelectorExpression prefixPostfixExpression parsedIdentifier
-}

arrowSelector :: PreprocessingParserX ()
arrowSelector = stringSatisfy_ (=="->")

postfixIncrementExpression :: PostfixExpression -> PreprocessingParserX PostfixExpression
postfixIncrementExpression prefixPostfixExpression = (parserFail "complexConditionalExpression not implemented") {->> do
    incrementOperator
    return $ PostfixIncrementExpression prefixPostfixExpression
-}

postfixDecrementExpression :: PostfixExpression -> PreprocessingParserX PostfixExpression
postfixDecrementExpression prefixPostfixExpression = (parserFail "complexConditionalExpression not implemented") {->> do
    decrementOperator
    return $ PostfixDecrementExpression prefixPostfixExpression
-}

primaryExpression :: PreprocessingParserX PrimaryExpression
primaryExpression =
    identifierPrimaryExpression <|>
    constantPrimaryExpression <|>
    stringLiteralPrimaryExpression <|>
    expressionPrimaryExpression <|>
    genericSelectionPrimaryExpression

identifierPrimaryExpression :: PreprocessingParserX PrimaryExpression
identifierPrimaryExpression = simpleExpression identifier IdentifierPrimaryExpression

constantPrimaryExpression :: PreprocessingParserX PrimaryExpression
constantPrimaryExpression = simpleExpression constant ConstantPrimaryExpression

stringLiteralPrimaryExpression :: PreprocessingParserX PrimaryExpression
stringLiteralPrimaryExpression = simpleExpression stringLiteral StringLiteralPrimaryExpression

expressionPrimaryExpression :: PreprocessingParserX PrimaryExpression
expressionPrimaryExpression = (parserFail "complexConditionalExpression not implemented") {->> simpleExpression (parens expression) ExpressionPrimaryExpression
-}

genericSelectionPrimaryExpression :: PreprocessingParserX PrimaryExpression
genericSelectionPrimaryExpression = (parserFail "complexConditionalExpression not implemented") {->> simpleExpression genericSelection GenericSelectionPrimaryExpression
-}

genericSelection :: PreprocessingParserX GenericSelection
genericSelection = (parserFail "complexConditionalExpression not implemented") {->> try $ do
    genericSelectionPrefix
    parens genericSelectionSuffix
-}

genericSelectionPrefix :: PreprocessingParserX ()
genericSelectionPrefix = (parserFail "complexConditionalExpression not implemented") {->> stringSatisfy_ (=="_Generic")
-}

genericSelectionSuffix :: PreprocessingParserX GenericSelection
genericSelectionSuffix = (parserFail "complexConditionalExpression not implemented") {->> try $ do
    parsedAssignmentExpression <- assignmentExpression
    parsedGenericAssocList <- genericAssocList
    return $ GenericSelection parsedAssignmentExpression parsedGenericAssocList
-}

genericAssocList :: PreprocessingParserX GenericAssocList
genericAssocList = (parserFail "complexConditionalExpression not implemented") {->> try $ do
    parsedGenericAssociations <- sepBy1NonConsumption genericAssociation comma
    return $ GenericAssocList . fromList parsedGenericAssociations
-}

genericAssociation :: PreprocessingParserX GenericAssociation
genericAssociation = (parserFail "complexConditionalExpression not implemented") {->> typedGenericAssociation <|> defaultGenericAssociation
-}

typedGenericAssociation :: PreprocessingParserX GenericAssociation
typedGenericAssociation = (parserFail "complexConditionalExpression not implemented") {->> try $ do
    parsedTypeName <- typeName
    colon
    parsedAssignmentExpression <- assignmentExpression
    return $ TypedGenericAssociation parsedTypeName parsedAssignmentExpression
-}

defaultGenericAssociation :: PreprocessingParserX GenericAssociation
defaultGenericAssociation = (parserFail "complexConditionalExpression not implemented") {->> try $ do
    default_
    colon
    parsedAssignmentExpression <- assignmentExpression
    return parsedAssignmentExpression
-}

default_ :: PreprocessingParserX ()
default_ = stringSatisfy_ (=="default")

typeName :: PreprocessingParserX TypeName
typeName = (parserFail "complexConditionalExpression not implemented") {->> tryWithFailMessage "Type Name" $ do
    parsedSpecifierQualifierList <- specifierQualifierList
    parsedMaybeAbstractDeclarator <- tryMaybe abstractDeclarator
    return $ TypeName parsedSpecifierQualifierList parsedMaybeAbstractDeclarator
-}

specifierQualifierList :: PreprocessingParserX SpecifierQualifierList
specifierQualifierList = (parserFail "complexConditionalExpression not implemented") {->> specifierSpecifierQualifierList <|> qualifierSpecifierQualifierList
-}

--specifierSpecifierQualifierList :: PreprocessingParserX SpecifierQualifierList
--specifierSpecifierQualifierList = try $ do

{-
integerConstant :: PreprocessingParserX Constant
integerConstant =
    decimalConstantIntegerConstant <|>
    octalConstantIntegerConstant <|>
    hexadecimalConstantIntegerConstant

hexadecimalConstantIntegerConstant :: PreprocessingParserX Constant
hexadecimalConstantIntegerConstant = do
    parsedHexadecimalConstant <- hexadecimalConstant
    parsedIntegerSuffix <- tryMaybe $ integerSuffix
    return $ HexadecimalConstantIntegerConstant parsedHexadecimalConstant parsedIntegerSuffix

integerSuffix :: PreprocessingParserX IntegerSuffix
integerSuffix =
    unsignedMaybeLong <|> -- check order for this
    unsignedLongLong <|>
    longMaybeUnsigned <|>
    longLongMaybeUnsigned

unsignedMaybeLong :: PreprocessingParserX IntegerSuffix
unsignedMaybeLong = do
    parsedUnsignedSuffix <- unsignedSuffix
    parsedLongSuffix <- tryMaybe $ longSuffix
    return $ UnsignedMaybeLong parsedUnsignedSuffix parsedLongSuffix

unsignedLongLong :: PreprocessingParserX IntegerSuffix
unsignedLongLong = do
    parsedUnsignedSuffix <- unsignedSuffix
    parsedLongLongSuffix <- longLongSuffix
    return $ UnsignedLongLong parsedUnsignedSuffix parsedLongLongSuffix

longMaybeUnsigned :: PreprocessingParserX IntegerSuffix
longMaybeUnsigned = do
    parsedLongSuffix <- longSuffix
    parsedMaybeUnsignedSuffix <- tryMaybe $ unsignedSuffix
    return $ LongMaybeUnsigned parsedLongSuffix parsedMaybeUnsignedSuffix

longLongMaybeUnsigned :: PreprocessingParserX IntegerSuffix
longLongMaybeUnsigned = do
    parsedLongLongSuffix <- longLongSuffix
    parsedMaybeUnsignedSuffix <- tryMaybe $ unsignedSuffix
    return $ LongLongMaybeUnsigned parsedLongLongSuffix parsedMaybeUnsignedSuffix

unsignedSuffix :: PreprocessingParserX UnsignedSuffix
unsignedSuffix = capitalUUnsignedSuffix <|> lowerCaseUUnsignedSuffix

capitalUUnsignedSuffix :: PreprocessingParserX UnsignedSuffix
capitalUUnsignedSuffix = simpleExpression (stringSatisfy_ (=="U")) CapitalUUnsignedSuffix

lowerCaseUUnsignedSuffix :: PreprocessingParserX UnsignedSuffix
lowerCaseUUnsignedSuffix = simpleExpression (stringSatisfy_ (=="u")) LowerCaseUUnsignedSuffix

longSuffix :: PreprocessingParserX LongSuffix
longSuffix = lowerCaseLLongSuffix <|> capitalLLongSuffix

lowerCaseLLongSuffix :: PreprocessingParserX LongSuffix
lowerCaseLLongSuffix = simpleExpression (stringSatisfy_ (=="l")) LowerCaseLLongSuffix

capitalLLongSuffix :: PreprocessingParserX LongSuffix
capitalLLongSuffix = simpleExpression (stringSatisfy_ (=="L")) CapitalLLongSuffix

longLongSuffix :: PreprocessingParserX LongLongSuffix
longLongSuffix = lowerCaseLLongLongSuffix <|> capitalLLongLongSuffix

lowerCaseLLongLongSuffix :: PreprocessingParserX LongLongSuffix
lowerCaseLLongLongSuffix = simpleExpression (stringSatisfy_ (=="ll")) LowerCaseLLongLongSuffix

capitalLLongLongSuffix :: PreprocessingParserX LongLongSuffix
capitalLLongLongSuffix = simpleExpression (stringSatisfy_ (=="LL")) CapitalLLongLongSuffix

hexadecimalConstant :: PreprocessingParserX HexadecimalConstant
hexadecimalConstant = try (charToStringTokenParser HexadecimalConstant.hexadecimalConstant) <?> "Hexadecimal Constant"
-}


