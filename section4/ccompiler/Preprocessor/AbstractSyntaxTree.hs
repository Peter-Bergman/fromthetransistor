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
    IfConstantExpression ConstantExpression (Maybe Group) |
    IfDef Identifier (Maybe Group) |
    IfNDef Identifier (Maybe Group)
    
type ElifGroups = [ElifGroup]

data ElifGroup = ElifGroup ConstantExpression (Maybe Group)

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

data ConstantExpression =
    ConditionalExpression

data ConditionalExpression =
    SimpleConditionalExpression LogicalOrExpression |
    ComplexConditionalExpression LogicalOrExpression ConstantExpression ConditionalExpression

data LogicalOrExpression =
    SimpleLogicalOrExpression LogicalAndExpression |
    ComplexLogicalOrExpression LogicalOrExpression LogicalAndExpression

data LogicalAndExpression =
    SimpleLogicalAndExpression InclusiveOrExpression |
    ComplexLogicalAndExpression LogicalAndExpression InclusiveOrExpression

data InclusiveOrExpression =
    SimpleInclusiveOrExpression ExclusiveOrExpression |
    ComplexInclusiveOrExpression InclusiveOrExpression ExclusiveOrExpression

data ExclusiveOrExpression =
    SimpleExclusiveOrExpression AndExpression |
    ComplexExclusiveOrExpression ExclusiveOrExpression AndExpression

data AndExpression =
    SimpleAndExpression EqualityExpression |
    ComplexAndExpression AndExpression EqualityExpression

data EqualityExpression =
    RelationalExpression RelationalExpression |
    EqualityExpression EqualityExpression RelationalExpression |
    InequalityExpression EqualityExpression RelationalExpression

data RelationalExpression =
    SimpleRelationalExpression ShiftExpression |
    LessThanExpression RelationalExpression ShiftExpression |
    GreaterThanExpression RelationalExpression ShiftExpression |
    LessThanOrEqualToExpression RelationalExpression ShiftExpression |
    GreaterThanOrEqualToExpression RelationalExpression ShiftExpression

data ShiftExpression =
    SimpleShiftExpression AdditiveExpression |
    LeftShiftExpression ShiftExpression AdditiveExpression |
    RightShiftExpression ShiftExpression AdditiveExpression

data AdditiveExpression =
    SimpleAdditiveExpression MultiplicativeExpression |
    AdditiveExpression AdditiveExpression MultiplicativeExpression |
    SubtractiveExpression AdditiveExpression MultiplicativeExpression

data MultiplicativeExpression =
    SimpleMultiplicativeExpression CastExpression |
    MultiplicativeExpression MultiplicativeExpression CastExpression |
    DivisionExpression MultiplicativeExpression CastExpression |
    ModuloExpression MultiplicativeExpression CastExpression

data CastExpression =
    SimpleCastExpression UnaryExpression |
    ComplexCastExpression TypeName CastExpression

data UnaryExpression =
    PostfixExpression PostfixExpression |
    IncrementExpression UnaryExpression |
    DecrementExpression UnaryExpression |
    OperatorExpression UnaryOperator CastExpression |
    SizeOfExpression UnaryExpression |
    SizeOfType TypeName |
    AlignOf TypeName

data UnaryOperator =
    Ampersand |
    Asterisk |
    PlusSign |
    MinusSign |
    Tilde |
    ExclamationPoint

data PostfixExpression =
    SimplePostfixExpression PrimaryExpression |
    IndexExpression PostfixExpression ConstantExpression |
    FunctionCallExpression PostfixExpression (Maybe ArgumentExpressionList) |
    DotSelectorExpression PostfixExpression Identifier |
    ArrowSelectorExpression PostfixExpression Identifier |
    PostfixIncrementExpression PostfixExpression |
    PostfixDecrementExpression PostfixExpression |
    PostfixInitializerListExpression TypeName InitializerList

data ArgumentExpressionList =
    SimpleArgumentExpressionList AssignmentExpression |
    ComplexExpressionList ArgumentExpressionList AssignmentExpression

data AssignmentExpression =
    SimpleAssignmentExpression ConditionalExpression |
    ComplexAssignmentExpression AssignmentExpression AssignmentOperator ConditionalExpression

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

data InitializerList =
    SimpleInitializerList (Maybe Designation) Initializer |
    ComplexInitializerList InitializerList (Maybe Designation) Initializer

type Designation = DesignatorList

data DesignatorList =
    SimpleDesignatorList Designator |
    ComplexDesignatorList DesignatorList Designator

data Designator =
    BracketDesignator ConstantExpression |
    DotDesignator Identifier

data Initializer =
    SimpleInitializer AssignmentExpression |
    ComplexInitializer InitializerList

data PrimaryExpression =
    IdentifierPrimaryExpression Identifier |
    ConstantPrimaryExpression Constant |
    StringLiteralPrimaryExpression String |
    ExpressionPrimaryExpression Expression |
    GenericSelectionPrimaryExpression GenericSelection

data Constant =
    IntegerConstant Integer |
    FloatingConstant Float |
    EnumerationConstant Int |
    CharacterConstant Char

data Expression =
    SimpleExpression AssignmentExpression |
    ComplexExpression Expression AssignmentExpression

data GenericSelection = GenericSelection AssignmentExpression GenericAssocList

data GenericAssocList =
    SimpleGenericAssocList GenericAssociation |
    ComplexGenericAssocList GenericAssocList GenericAssociation

data GenericAssociation =
    TypedGenericAssociation TypeName AssignmentExpression |
    DefaultGenericAssociation AssignmentExpression

data TypeName = TypeName SpecifierQualifierList (Maybe AbstractDeclarator)

data AbstractDeclarator =
    SimpleAbstractDeclarator Pointer |
    ComplexAbstractDeclarator (Maybe Pointer) DirectAbstractDeclarator

data Pointer =
    SimplePointer (Maybe TypeQualifierList) |
    ComplexPointer (Maybe TypeQualifierList) Pointer

data DirectAbstractDeclarator =
    SimpleDirectAbstractDeclarator AbstractDeclarator |
    MaybeAssignmentDirectAbstractDeclarator (Maybe DirectAbstractDeclarator) (Maybe TypeQualifierList) (Maybe AssignmentExpression) |
    StaticFirstDirectAbstractDeclarator (Maybe DirectAbstractDeclarator) (Maybe TypeQualifierList) AssignmentExpression |
    TypeQualifierListDirectAbstractDeclarator (Maybe DirectAbstractDeclarator) TypeQualifierList AssignmentExpression |
    AsteriskDirectAbstractDeclarator (Maybe DirectAbstractDeclarator) |
    ParameterListTypeDirectAbstractDeclarator (Maybe DirectAbstractDeclarator) (Maybe ParameterTypeList)

data ParameterTypeList =
    SimpleParameterTypeList ParameterList |
    EllipsisParameterTypeList ParameterList

data ParameterList =
    SimpleParameterList ParameterDeclaration |
    ComplexParameterList ParameterList ParameterDeclaration

data ParameterDeclaration =
    SimpleParameterDeclaration DeclarationSpecifiers Declarator |
    AbstractParameterDeclaration DeclarationSpecifiers (Maybe AbstractDeclarator)

data Declarator = Declarator (Maybe Pointer) DirectDeclarator

data DirectDeclarator =
    IdentifierDirectDeclarator Identifier |
    SimpleDirectDeclarator Declarator |
    MaybeAssignmentDirectDeclarator DirectDeclarator (Maybe TypeQualifierList) (Maybe AssignmentExpression) |
    StaticFirstDirectDeclarator DirectDeclarator (Maybe TypeQualifierList) AssignmentExpression |
    TypeQualifierListDirectDeclarator DirectDeclarator (Maybe TypeQualifierList) |
    AsteriskDirectDeclarator DirectDeclarator (Maybe TypeQualifierList) |
    ParameterListDirectDeclarator (Maybe ParameterTypeList) |
    IdentifierListDirectDeclarator DirectDeclarator (Maybe IdentifierList)


data SpecifierQualifierList =
    SpecifierSpecifierQualifierList TypeSpecifier SpecifierQualifierList |
    QualifierSpecifierQualifierList TypeQualifier SpecifierQualifierList

data TypeSpecifier =
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
    AtomicTypeSpecifierTypeSpecifier AtomicTypeSpecifier |
    StructOrUnionSpecifier |
    EnumSpecifier |
    TypeDefNameSpecifier

newtype AtomicTypeSpecifier = AtomicTypeSpecifier TypeName

data StructOrUnionSpecifier =
    IdentifierStructOrUnionSpecifier StructOrUnion Identifier |
    BracesStructOrUnionSpecifier (Maybe Identifier) StructDeclarationList

data StructOrUnion =
    Struct |
    Union

type StructDeclarationList = NonEmptyList StructDeclaration

data StructDeclaration =
    SpecifierQualifierListStructDeclaration SpecifierQualifierList (Maybe StructDeclaratorList) |
    AssertStructDeclaration StaticAssertDeclaration

data StaticAssertDeclaration = StaticAssertDeclaration ConstantExpression String

type StructDeclaratorList = NonEmptyList StructDeclarator

data StructDeclarator =
    SimpleStructDeclarator Declarator |
    ComplexStructDeclarator (Maybe Declarator) ConstantExpression

type TypeQualifierList = NonEmptyList TypeQualifier

data TypeQualifier =
    Const |
    Restrict |
    Volatile |
    Atomic

data DeclarationSpecifiers =
    StorageClassDeclarationSpecifiers StorageClassSpecifier (Maybe DeclarationSpecifiers) |
    TypeSpecifierDeclarationSpecifiers TypeSpecifier (Maybe DeclarationSpecifiers) |
    TypeQualifierDeclarationSpecifiers TypeQualifier (Maybe DeclarationSpecifiers) |
    FunctionSpecifierDeclarationSpecifiers FunctionSpecifier (Maybe DeclarationSpecifiers) |
    AlignmentDeclarationSpecifiers AlignmentSpecifier (Maybe DeclarationSpecifiers)

data StorageClassSpecifier =
    TypeDef |
    Extern |
    Static |
    ThreadLocal |
    Auto |
    Register

data FunctionSpecifier =
    Inline |
    NoReturn

data AlignmentSpecifier =
    TypeNameAlignment TypeName |
    ConstantAlignment ConstantExpression


