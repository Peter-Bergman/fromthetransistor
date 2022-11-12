module AbstractSyntaxTree where

-- Implementation Specific data constructor
-- Could use for refactors in a bit
data NonEmptyList a =
    Element a |
    Elements (NonEmptyList a) a

type AbstractSyntaxTree = Maybe Group
type Group = [GroupPart]

data GroupPart = 
    IfSection IfSection |
    ControlLine |
    TextLine |
    NonDirective

data IfSection =
    IfGroup IfGroup |
    ElifGroups (Maybe ElifGroups) |
    ElseGroup (Maybe ElseGroup)

data IfGroup = 
    IfConstantExpression ConstantExpressionC (Maybe Group) |
    IfDef Identifier (Maybe Group) |
    IfNDef Identifier (Maybe Group)
    
type ElifGroups = [ElifGroup]

data ElifGroup = ElifGroup ConstantExpressionC (Maybe Group)

type ElseGroup = Maybe Group

type Identifier = String

data ControlLine =
    IncludeDirective PPTokens |
    DefineDirective DefineDirective |
    UndefDirective Identifier |
    LineDirective PPTokens |
    ErrorDirective (Maybe PPTokens) |
    PragmaDirective (Maybe PPTokens) |
    NullDirective

type PPTokens = [String]

data DefineDirective =
    SimpleDefine Identifier ReplacementList |
    FunctionDefine Identifier (Maybe IdentifierList) ReplacementList |
    EllipsisFunctionDefine Identifier ReplacementList |
    ComplexDefine Identifier IdentifierList ReplacementList

type ReplacementList = Maybe PPTokens
type IdentifierList = [Identifier]

data ConstantExpressionC =
    ConditionalExpressionC

data ConditionalExpressionC =
    SimpleConditionalExpressionC LogicalOrExpressionC |
    ComplexConditionalExpressionC LogicalOrExpressionC ConstantExpressionC ConditionalExpressionC

data LogicalOrExpressionC =
    SimpleLogicalOrExpressionC LogicalAndExpressionC |
    ComplexLogicalOrExpressionC LogicalOrExpressionC LogicalAndExpressionC

data LogicalAndExpressionC =
    SimpleLogicalAndExpressionC InclusiveOrExpressionC |
    ComplexLogicalAndExpressionC LogicalAndExpressionC InclusiveOrExpressionC

data InclusiveOrExpressionC =
    SimpleInclusiveOrExpressionC ExclusiveOrExpressionC |
    ComplexInclusiveOrExpressionC InclusiveOrExpressionC ExclusiveOrExpressionC

data ExclusiveOrExpressionC =
    SimpleExclusiveOrExpressionC AndExpressionC |
    ComplexExclusiveOrExpressionC ExclusiveOrExpressionC AndExpressionC

data AndExpressionC =
    SimpleAndExpressionC EqualityExpressionC |
    ComplexAndExpressionC AndExpressionC EqualityExpressionC

data EqualityExpressionC =
    RelationalExpressionC RelationalExpressionC |
    EqualityExpressionC EqualityExpressionC RelationalExpressionC |
    InequalityExpressionC EqualityExpressionC RelationalExpressionC

data RelationalExpressionC =
    SimpleRelationalExpressionC ShiftExpressionC |
    LessThanExpressionC RelationalExpressionC ShiftExpressionC |
    GreaterThanExpressionC RelationalExpressionC ShiftExpressionC |
    LessThanOrEqualToExpressionC RelationalExpressionC ShiftExpressionC |
    GreaterThanOrEqualToExpressionC RelationalExpressionC ShiftExpressionC

data ShiftExpressionC =
    SimpleShiftExpressionC AdditiveExpressionC |
    LeftShiftExpressionC ShiftExpressionC AdditiveExpressionC |
    RightShiftExpressionC ShiftExpressionC AdditiveExpressionC

data AdditiveExpressionC =
    SimpleAdditiveExpressionC MultiplicativeExpressionC |
    AdditiveExpressionC AdditiveExpressionC MultiplicativeExpressionC |
    SubtractiveExpressionC AdditiveExpressionC MultiplicativeExpressionC

data MultiplicativeExpressionC =
    SimpleMultiplicativeExpressionC CastExpressionC |
    MultiplicativeExpressionC MultiplicativeExpressionC CastExpressionC |
    DivisionExpressionC MultiplicativeExpressionC CastExpressionC |
    ModuloExpression MultiplicativeExpressionC CastExpressionC

data CastExpressionC =
    SimpleCastExpressionC UnaryExpressionC |
    ComplexCastExpressionC TypeName CastExpressionC

data UnaryExpressionC =
    PostfixExpressionC PostfixExpressionC |
    IncrementExpressionC UnaryExpressionC |
    DecrementExpressionC UnaryExpressionC |
    OperatorExpressionC UnaryOperator CastExpressionC |
    SizeOfExpressionC UnaryExpressionC |
    SizeOfTypeC TypeName |
    AlignOfC TypeName

data UnaryOperator =
    Ampersand |
    Asterisk |
    PlusSign |
    MinusSign |
    Tilde |
    ExclamationPoint

data PostfixExpressionC =
    SimplePostfixExpressionC PrimaryExpressionC |
    IndexExpression PostfixExpressionC ConstantExpressionC |
    FunctionCallExpression PostfixExpressionC (Maybe ArgumentExpressionListC) |
    DotSelectorExpression PostfixExpressionC Identifier |
    ArrowSelectorExpression PostfixExpressionC Identifier |
    PostfixIncrementExpressionC PostfixExpressionC |
    PostfixDecrementExpressionC PostfixExpressionC |
    PostfixInitializerListExpressionC TypeName InitializerListC

data ArgumentExpressionListC =
    SimpleArgumentExpressionListC AssignmentExpressionC |
    ComplexExpressionListC ArgumentExpressionListC AssignmentExpressionC

data AssignmentExpressionC =
    SimpleAssignmentExpressionC ConditionalExpressionC |
    ComplexAssignmentExpressionC AssignmentExpressionC AssignmentOperator ConditionalExpressionC

data AssignmentOperator =
    EqualSign |
    AsteriskEqual |
    SlashEqual |
    PercentEqual |
    PlusEqual |
    MinusEqual |
    LeftShiftEqual |
    RightShiftEqual |
    AmpersandEqual |
    CaretEqual |
    PipeEqual

data InitializerListC =
    SimpleInitializerListC (Maybe DesignationC) InitializerC |
    ComplexInitializerListC InitializerListC (Maybe DesignationC) InitializerC

type DesignationC = DesignatorListC

data DesignatorListC =
    SimpleDesignatorListC DesignatorC |
    ComplexDesignatorListC DesignatorListC DesignatorC

data DesignatorC =
    BracketDesignator ConstantExpressionC |
    DotDesignator Identifier

data InitializerC =
    SimpleInitializerC AssignmentExpressionC |
    ComplexInitializerC InitializerListC

data PrimaryExpressionC =
    IdentifierPrimaryExpressionC Identifier |
    ConstantPrimaryExpressionC Constant |
    StringLiteralPrimaryExpressionC String |
    ExpressionPrimaryExpressionC ExpressionC |
    GenericSelectionPrimaryExpressionC GenericSelectionC

data Constant =
    IntegerConstant Integer |
    FloatingConstant Float |
    EnumerationConstant Int |
    CharacterConstant Char

data ExpressionC =
    SimpleExpressionC AssignmentExpressionC |
    ComplexExpressionC ExpressionC AssignmentExpressionC

newtype GenericSelectionC = GenericSelectionC AssignmentExpressionC GenericAssocListC

data GenericAssocListC =
    SimpleGenericAssocListC GenericAssociationC |
    ComplexGenericAssocListC GenericAssocListC GenericAssociationC

data GenericAssociationC =
    TypedGenericAssociation TypeName AssignmentExpressionC |
    DefaultGenericAssociation AssignmentExpressionC

newtype TypeName = TypeName SpecifierQualifierListC (Maybe AbstractDeclaratorC)

data AbstractDeclaratorC =
    SimpleAbstractDeclaratorC PointerC |
    ComplexAbstractDeclaratorC (Maybe PointerC) DirectAbstractDeclaratorC

data PointerC =
    SimplePointerC (Maybe TypeQualifierListC) |
    ComplexPointerC (Maybe TypeQualifierListC) PointerC

data DirectAbstractDeclaratorC =
    SimpleDirectAbstractDeclaratorC AbstractDeclaratorC |
    MaybeAssignmentDirectAbstractDeclaratorC (Maybe DirectAbstractDeclaratorC) (Maybe TypeQualifierListC) (Maybe AssignmentExpressionC) |
    StaticFirstDirectAbstractDeclaratorC (Maybe DirectAbstractDeclaratorC) (Maybe TypeQualifierListC) AssignmentExpressionC |
    TypeQualifierListDirectAbstractDeclaratorC (Maybe DirectAbstractDeclaratorC) TypeQualifierListC AssignmentExpressionC |
    AsteriskDirectAbstractDeclaratorC (Maybe DirectAbstractDeclaratorC) |
    ParameterListTypeDirectAbstractDeclaratorC (Maybe DirectAbstractDeclaratorC) (Maybe ParameterTypeListC)

data ParameterTypeListC =
    SimpleParameterTypeListC ParameterListC |
    EllipsisParameterTypeListC ParameterListC

data ParameterListC =
    SimpleParameterListC ParameterDeclarationC |
    ComplexParameterListC ParameterListC ParameterDeclarationC

data ParameterDeclarationC =
    SimpleParameterDeclarationC DeclarationSpecifiersC DeclaratorC |
    AbstractParameterDeclarationC DeclarationSpecifiersC (Maybe AbstractDeclaratorC)

newtype DeclaratorC = DeclaratorC (Maybe PointerC) DirectDeclaratorC

data DirectDeclaratorC =
    IdentifierDirectDeclarator Identifier |
    SimpleDirectDeclarator DeclaratorC |
    MaybeAssignmentDirectDeclarator DirectDeclaratorC (Maybe TypeQualifierListC) (Maybe AssignmentExpressionC) |
    StaticFirstDirectDeclaratorC DirectDeclaratorC (Maybe TypeQualifierListC) AssignmentExpressionC |
    TypeQualifierListDirectDeclaratorC DirectDeclaratorC (Maybe TypeQualifierListC) |
    AsteriskDirectDeclaratorC DirectDeclaratorC (Maybe TypeQualifierListC) |
    ParameterListDirectDeclaratorC (Maybe ParameterTypeListC) |
    IdentifierListDirectDeclaratorC DirectDeclaratorC (Maybe IdentifierList)


data SpecifierQualifierListC =
    SpecifierSpecifierQualifierListC TypeSpecifierC SpecifierQualifierListC |
    QualifierSpecifierQualifierListC TypeQualifierC SpecifierQualifierListC

data TypeSpecifierC =
    Void |
    Char_ |
    Short |
    Int_ |
    Long |
    Float_ |
    Double_ |
    Signed |
    Unsigned |
    Bool_ |
    Complex_ |
    AtomicTypeSpecifierTypeSpecifier AtomicTypeSpecifierC |
    StructOrUnionSpecifierC |
    EnumSpecifierC |
    TypeDefNameSpecifierC

newtype AtomicTypeSpecifierC = AtomicTypeSpecifierC TypeName

data StructOrUnionSpecifierC =
    IdentifierStructOrUnionSpecifierC StructOrUnion Identifier |
    BracesStructOrUnionSpecifierC (Maybe Identifier) StructDeclarationListC

data StructOrUnion =
    Struct |
    Union

type StructDeclarationList = NonEmptyList StructDeclaration

data StructDeclaration =
    SpecifierQualifierListStructDeclaration SpecifierQualifierListC (Maybe StructDeclaratorList) |
    AssertStructDeclaration StaticAssertDeclaration


