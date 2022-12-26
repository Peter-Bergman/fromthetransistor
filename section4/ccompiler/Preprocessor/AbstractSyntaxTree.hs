module AbstractSyntaxTree where
import Data.List.NonEmpty

type AbstractSyntaxTree = Maybe Group
type Group = NonEmpty GroupPart

data GroupPart =
    IfSection IfGroup (Maybe ElifGroups) (Maybe ElseGroup) |
    ControlLine ControlLine |
    TextLine (Maybe PPTokens) |
    NonDirective PPTokens
    deriving (Show)

data IfGroup =
    IfDirective ConstantExpression (Maybe Group) |
    IfDefDirective Identifier (Maybe Group) |
    IfNDefDirective Identifier (Maybe Group)
    deriving (Show)

type ElifGroups = NonEmpty ElifGroup

data ElifGroup =
    ElifGroup ConstantExpression (Maybe Group)
    deriving (Show)

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
    deriving (Show)

type PPToken = String
type PPTokens = NonEmpty PPToken

data DefineDirective =
    ObjectLikeDefine Identifier ReplacementList |
    FunctionDefine Identifier (Maybe IdentifierList) ReplacementList |
    EllipsisFunctionDefine Identifier ReplacementList |
    IdentifierListEllipsisFunctionDefine Identifier IdentifierList ReplacementList
    deriving (Show)

type ReplacementList = Maybe PPTokens
type IdentifierList = NonEmpty Identifier

-- This has to be revised. There needs to be a constructor that constructs a ConstantExpression with a ConditionalExpression
newtype ConstantExpression =
    ConstantExpression Integer -- remove this line and uncomment line below. THIS IS JUST FOR TESTING
    --ConditionalExpression ConditionalExpression
    deriving (Show)

data ConditionalExpression =
    SimpleConditionalExpression LogicalOrExpression |
    ComplexConditionalExpression LogicalOrExpression ConstantExpression ConditionalExpression
    deriving (Show)

data LogicalOrExpression =
    SimpleLogicalOrExpression LogicalAndExpression |
    ComplexLogicalOrExpression LogicalOrExpression LogicalAndExpression
    deriving (Show)

data LogicalAndExpression =
    SimpleLogicalAndExpression InclusiveOrExpression |
    ComplexLogicalAndExpression LogicalAndExpression InclusiveOrExpression
    deriving (Show)

data InclusiveOrExpression =
    SimpleInclusiveOrExpression ExclusiveOrExpression |
    ComplexInclusiveOrExpression InclusiveOrExpression ExclusiveOrExpression
    deriving (Show)

data ExclusiveOrExpression =
    SimpleExclusiveOrExpression AndExpression |
    ComplexExclusiveOrExpression ExclusiveOrExpression AndExpression
    deriving (Show)

data AndExpression =
    SimpleAndExpression EqualityExpression |
    ComplexAndExpression AndExpression EqualityExpression
    deriving (Show)

data EqualityExpression =
    RelationalExpression RelationalExpression |
    EqualityExpression EqualityExpression RelationalExpression |
    InequalityExpression EqualityExpression RelationalExpression
    deriving (Show)

data RelationalExpression =
    SimpleRelationalExpression ShiftExpression |
    LessThanExpression RelationalExpression ShiftExpression |
    GreaterThanExpression RelationalExpression ShiftExpression |
    LessThanOrEqualToExpression RelationalExpression ShiftExpression |
    GreaterThanOrEqualToExpression RelationalExpression ShiftExpression
    deriving (Show)

data ShiftExpression =
    SimpleShiftExpression AdditiveExpression |
    LeftShiftExpression ShiftExpression AdditiveExpression |
    RightShiftExpression ShiftExpression AdditiveExpression
    deriving (Show)

data AdditiveExpression =
    SimpleAdditiveExpression MultiplicativeExpression |
    AdditiveExpression AdditiveExpression MultiplicativeExpression |
    SubtractiveExpression AdditiveExpression MultiplicativeExpression
    deriving (Show)

data MultiplicativeExpression =
    SimpleMultiplicativeExpression CastExpression |
    MultiplicativeExpression MultiplicativeExpression CastExpression |
    DivisionExpression MultiplicativeExpression CastExpression |
    ModuloExpression MultiplicativeExpression CastExpression
    deriving (Show)

data CastExpression =
    SimpleCastExpression UnaryExpression |
    ComplexCastExpression TypeName CastExpression
    deriving (Show)

data UnaryExpression =
    PostfixExpression PostfixExpression |
    IncrementExpression UnaryExpression |
    DecrementExpression UnaryExpression |
    OperatorExpression UnaryOperator CastExpression |
    SizeOfExpression UnaryExpression |
    SizeOfType TypeName |
    AlignOf TypeName |
    Defined Identifier -- The Defined constructor is only for constant expressions handled by the preprocessor
    deriving (Show)

data UnaryOperator =
    Ampersand |
    Asterisk |
    PlusSign |
    MinusSign |
    Tilde |
    ExclamationPoint
    deriving (Show)

data PostfixExpression =
    SimplePostfixExpression PrimaryExpression |
    IndexExpression PostfixExpression ConstantExpression |
    FunctionCallExpression PostfixExpression (Maybe ArgumentExpressionList) |
    DotSelectorExpression PostfixExpression Identifier |
    ArrowSelectorExpression PostfixExpression Identifier |
    PostfixIncrementExpression PostfixExpression |
    PostfixDecrementExpression PostfixExpression |
    PostfixInitializerListExpression TypeName InitializerList
    deriving (Show)

data ArgumentExpressionList =
    SimpleArgumentExpressionList AssignmentExpression |
    ComplexExpressionList ArgumentExpressionList AssignmentExpression
    deriving (Show)

data AssignmentExpression =
    SimpleAssignmentExpression ConditionalExpression |
    ComplexAssignmentExpression AssignmentExpression AssignmentOperator ConditionalExpression
    deriving (Show)

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
    deriving (Show)

data InitializerList =
    SimpleInitializerList (Maybe Designation) Initializer |
    ComplexInitializerList InitializerList (Maybe Designation) Initializer
    deriving (Show)

type Designation = DesignatorList

data DesignatorList =
    SimpleDesignatorList Designator |
    ComplexDesignatorList DesignatorList Designator
    deriving (Show)

data Designator =
    BracketDesignator ConstantExpression |
    DotDesignator Identifier
    deriving (Show)

data Initializer =
    SimpleInitializer AssignmentExpression |
    ComplexInitializer InitializerList
    deriving (Show)

data PrimaryExpression =
    IdentifierPrimaryExpression Identifier |
    ConstantPrimaryExpression Constant |
    StringLiteralPrimaryExpression String |
    ExpressionPrimaryExpression Expression |
    GenericSelectionPrimaryExpression GenericSelection
    deriving (Show)

data Constant =
    IntegerConstant Integer |
    FloatingConstant Float |
    EnumerationConstant Int |
    CharacterConstant Char
    deriving (Show)

data Expression =
    SimpleExpression AssignmentExpression |
    ComplexExpression Expression AssignmentExpression
    deriving (Show)

data GenericSelection =
    GenericSelection AssignmentExpression GenericAssocList
    deriving Show

data GenericAssocList =
    SimpleGenericAssocList GenericAssociation |
    ComplexGenericAssocList GenericAssocList GenericAssociation
    deriving (Show)

data GenericAssociation =
    TypedGenericAssociation TypeName AssignmentExpression |
    DefaultGenericAssociation AssignmentExpression
    deriving (Show)

data TypeName =
    TypeName SpecifierQualifierList (Maybe AbstractDeclarator)
    deriving (Show)

data AbstractDeclarator =
    SimpleAbstractDeclarator Pointer |
    ComplexAbstractDeclarator (Maybe Pointer) DirectAbstractDeclarator
    deriving (Show)

data Pointer =
    SimplePointer (Maybe TypeQualifierList) |
    ComplexPointer (Maybe TypeQualifierList) Pointer
    deriving (Show)

data DirectAbstractDeclarator =
    SimpleDirectAbstractDeclarator AbstractDeclarator |
    MaybeAssignmentDirectAbstractDeclarator (Maybe DirectAbstractDeclarator) (Maybe TypeQualifierList) (Maybe AssignmentExpression) |
    StaticFirstDirectAbstractDeclarator (Maybe DirectAbstractDeclarator) (Maybe TypeQualifierList) AssignmentExpression |
    TypeQualifierListDirectAbstractDeclarator (Maybe DirectAbstractDeclarator) TypeQualifierList AssignmentExpression |
    AsteriskDirectAbstractDeclarator (Maybe DirectAbstractDeclarator) |
    ParameterListTypeDirectAbstractDeclarator (Maybe DirectAbstractDeclarator) (Maybe ParameterTypeList)
    deriving (Show)

data ParameterTypeList =
    SimpleParameterTypeList ParameterList |
    EllipsisParameterTypeList ParameterList
    deriving (Show)

data ParameterList =
    SimpleParameterList ParameterDeclaration |
    ComplexParameterList ParameterList ParameterDeclaration
    deriving (Show)

data ParameterDeclaration =
    SimpleParameterDeclaration DeclarationSpecifiers Declarator |
    AbstractParameterDeclaration DeclarationSpecifiers (Maybe AbstractDeclarator)
    deriving (Show)

data Declarator =
    Declarator (Maybe Pointer) DirectDeclarator
    deriving (Show)

data DirectDeclarator =
    IdentifierDirectDeclarator Identifier |
    SimpleDirectDeclarator Declarator |
    MaybeAssignmentDirectDeclarator DirectDeclarator (Maybe TypeQualifierList) (Maybe AssignmentExpression) |
    StaticFirstDirectDeclarator DirectDeclarator (Maybe TypeQualifierList) AssignmentExpression |
    TypeQualifierListDirectDeclarator DirectDeclarator (Maybe TypeQualifierList) |
    AsteriskDirectDeclarator DirectDeclarator (Maybe TypeQualifierList) |
    ParameterListDirectDeclarator (Maybe ParameterTypeList) |
    IdentifierListDirectDeclarator DirectDeclarator (Maybe IdentifierList)
    deriving (Show)


data SpecifierQualifierList =
    SpecifierSpecifierQualifierList TypeSpecifier SpecifierQualifierList |
    QualifierSpecifierQualifierList TypeQualifier SpecifierQualifierList
    deriving (Show)

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
    deriving (Show)

newtype AtomicTypeSpecifier =
    AtomicTypeSpecifier TypeName
    deriving (Show)

data StructOrUnionSpecifier =
    IdentifierStructOrUnionSpecifier StructOrUnion Identifier |
    BracesStructOrUnionSpecifier (Maybe Identifier) StructDeclarationList
    deriving (Show)

data StructOrUnion =
    Struct |
    Union
    deriving (Show)

type StructDeclarationList = NonEmpty StructDeclaration

data StructDeclaration =
    SpecifierQualifierListStructDeclaration SpecifierQualifierList (Maybe StructDeclaratorList) |
    AssertStructDeclaration StaticAssertDeclaration
    deriving (Show)

data StaticAssertDeclaration =
    StaticAssertDeclaration ConstantExpression String
    deriving (Show)

type StructDeclaratorList = NonEmpty StructDeclarator

data StructDeclarator =
    SimpleStructDeclarator Declarator |
    ComplexStructDeclarator (Maybe Declarator) ConstantExpression
    deriving (Show)

type TypeQualifierList = NonEmpty TypeQualifier

data TypeQualifier =
    Const |
    Restrict |
    Volatile |
    Atomic
    deriving (Show)

data DeclarationSpecifiers =
    StorageClassDeclarationSpecifiers StorageClassSpecifier (Maybe DeclarationSpecifiers) |
    TypeSpecifierDeclarationSpecifiers TypeSpecifier (Maybe DeclarationSpecifiers) |
    TypeQualifierDeclarationSpecifiers TypeQualifier (Maybe DeclarationSpecifiers) |
    FunctionSpecifierDeclarationSpecifiers FunctionSpecifier (Maybe DeclarationSpecifiers) |
    AlignmentDeclarationSpecifiers AlignmentSpecifier (Maybe DeclarationSpecifiers)
    deriving (Show)

data StorageClassSpecifier =
    TypeDef |
    Extern |
    Static |
    ThreadLocal |
    Auto |
    Register
    deriving (Show)

data FunctionSpecifier =
    Inline |
    NoReturn
    deriving (Show)

data AlignmentSpecifier =
    TypeNameAlignment TypeName |
    ConstantAlignment ConstantExpression
    deriving (Show)


