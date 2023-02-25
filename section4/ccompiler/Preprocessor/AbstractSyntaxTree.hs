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

newtype ConstantExpression =
    ConditionalExpression ConditionalExpression
    deriving (Show)

data ConditionalExpression =
    SimpleConditionalExpression LogicalOrExpression |
    ComplexConditionalExpression LogicalOrExpression ConstantExpression ConditionalExpression
    deriving (Show)

newtype LogicalOrExpression = 
    LogicalOrExpression (NonEmpty LogicalAndExpression)
    deriving (Show)

newtype LogicalAndExpression =
    LogicalAndExpression  (NonEmpty InclusiveOrExpression)
    deriving (Show)

newtype InclusiveOrExpression =
    InclusiveOrExpression (NonEmpty ExclusiveOrExpression)
    deriving (Show)

newtype ExclusiveOrExpression =
    ExclusiveOrExpression (NonEmpty AndExpression)
    deriving (Show)

data AndExpression =
    AndExpression (NonEmpty EqualityExpression)
    deriving (Show)

data EqualityExpression =
    SimpleEqualityExpression RelationalExpression |
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
    AdditionExpression AdditiveExpression MultiplicativeExpression |
    SubtractionExpression AdditiveExpression MultiplicativeExpression
    deriving (Show)

data MultiplicativeExpression =
    SimpleMultiplicativeExpression CastExpression |
    MultiplicationExpression MultiplicativeExpression CastExpression |
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
    AlignOfType TypeName |
    DefinedWithParentheses Identifier -- The DefinedWithParentheses constructor is only for constant expressions handled by the preprocessor
    deriving (Show)

data UnaryOperator =
    AddressOf |
    Indirection |
    PlusSignOperator |
    MinusSignOperator |
    BitwiseNot |
    LogicalNot |
    DefinedWithoutParentheses -- The DefinedOperator constructor is only for constant expressions handled by the preprocessor
    deriving (Show)

data PostfixExpression =
    SimplePostfixExpression PrimaryExpression |
    IndexExpression PostfixExpression Expression |
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

newtype InitializerList = InitializerList (NonEmpty (Maybe Designation, Initializer))
    deriving (Show)

newtype Designation =
    Designation DesignatorList
    deriving (Show)

newtype DesignatorList =
    DesignatorList (NonEmpty Designator)
    deriving (Show)

data Designator =
    BracketDesignator ConstantExpression |
    DotDesignator Identifier
    deriving (Show)

data Initializer =
    AssignmentExpressionInitializer AssignmentExpression |
    InitializerListInitializer InitializerList
    deriving (Show)

data PrimaryExpression =
    IdentifierPrimaryExpression Identifier |
    ConstantPrimaryExpression Constant |
    StringLiteralPrimaryExpression String |
    ExpressionPrimaryExpression Expression |
    GenericSelectionPrimaryExpression GenericSelection
    deriving (Show)

data Constant =
    IntegerConstantConstant IntegerConstant |
    FloatingConstantConstant FloatingConstant |
    EnumerationConstantConstant EnumerationConstant |
    CharacterConstantConstant CharacterConstant
    deriving (Show)

data IntegerConstant =
    DecimalConstantIntegerConstant DecimalConstant (Maybe IntegerSuffix) |
    OctalConstantIntegerConstant OctalConstant (Maybe IntegerSuffix) |
    HexadecimalConstantIntegerConstant HexadecimalConstant (Maybe IntegerSuffix)
    deriving (Show)

data DecimalConstant =
    DecimalConstant NonzeroDigit [Digit]
    deriving (Show)

newtype OctalConstant =
    OctalConstant [OctalDigit]
    deriving (Show)

data HexadecimalConstant =
    HexadecimalConstant HexadecimalPrefix (NonEmpty HexadecimalDigit)
    deriving (Show)

data HexadecimalPrefix =
    CapitalXHexPrefix |
    LowerCaseXHexPrefix
    deriving (Show)

data IntegerSuffix =
    UnsignedMaybeLong UnsignedSuffix (Maybe LongSuffix) |
    UnsignedLongLong UnsignedSuffix LongLongSuffix |
    LongMaybeUnsigned LongSuffix (Maybe UnsignedSuffix) |
    LongLongMaybeUnsigned LongLongSuffix (Maybe UnsignedSuffix)
    deriving (Show)

newtype NonzeroDigit =
    NonzeroDigit Integer
    deriving (Show)

newtype Digit =
    Digit Integer
    deriving (Show)

newtype OctalDigit =
    OctalDigit Integer
    deriving (Show)

-- maybe this should be an isomorphic type of Integer like Digit and OctalDigit
newtype HexadecimalDigit =
    HexadecimalDigit Char
    deriving (Show)

data UnsignedSuffix =
    CapitalUUnsignedSuffix |
    LowerCaseUUnsignedSuffix
    deriving (Show)

data LongSuffix =
    CapitalLLongSuffix |
    LowerCaseLLongSuffix
    deriving (Show)

data LongLongSuffix =
    CapitalLLongLongSuffix |
    LowerCaseLLongLongSuffix
    deriving (Show)

data FloatingConstant =
    DecimalFloatingConstant DecimalFloatingConstant |
    HexadecimalFloatingConstant HexadecimalFloatingConstant
    deriving (Show)

data DecimalFloatingConstant =
    FractionalDecimalFloatingConstant FractionalConstant (Maybe ExponentPart) (Maybe FloatingSuffix) |
    DigitSequenceDecimalFloatingConstant DigitSequence ExponentPart (Maybe FloatingSuffix)
    deriving (Show)

data FractionalConstant =
    HighPrecisionFractionalConstant (Maybe DigitSequence) DigitSequence |
    LowPrecisionFractionalConstant DigitSequence
    deriving (Show)

data ExponentPart =
    LowerCaseEExponentPart (Maybe Sign) DigitSequence |
    CapitalEExponentPart (Maybe Sign) DigitSequence
    deriving (Show)

data Sign =
    PlusSign |
    MinusSign
    deriving (Show)

newtype DigitSequence =
    DigitSequence (NonEmpty Digit)
    deriving (Show)

data FloatingSuffix =
    LowerCaseFFloatingSuffix |
    LowerCaseLFloatingSuffix |
    CapitalFFloatingSuffix |
    CapitalLFloatingSuffix
    deriving (Show)

data HexadecimalFloatingConstant =
    FractionalHexadecimalFloatingConstant HexadecimalPrefix HexadecimalFractionalConstant BinaryExponentPart (Maybe FloatingSuffix) |
    DigitSequenceHexadecimalFloatingConstant HexadecimalPrefix HexadecimalDigitSequence BinaryExponentPart (Maybe FloatingSuffix)
    deriving (Show)

data HexadecimalFractionalConstant =
    HighPrecisionHexadecimalFractionalConstant (Maybe HexadecimalDigitSequence) HexadecimalDigitSequence |
    LowPrecisionHexadecimalFractionalConstant HexadecimalDigitSequence
    deriving (Show)

newtype HexadecimalDigitSequence =
    HexadecimalDigitSequence (NonEmpty HexadecimalDigit)
    deriving (Show)

data BinaryExponentPart =
    LowerCasePExponentPart (Maybe Sign) DigitSequence |
    CapitalPExponentPart (Maybe Sign) DigitSequence
    deriving (Show)

newtype EnumerationConstant =
    EnumerationConstant Identifier
    deriving (Show)

data CharacterConstant =
    SimpleCharacterConstant CCharSequence |
    LCharacterConstant CCharSequence |
    LowerCaseUCharacterConstant CCharSequence |
    CapitalUCharacterConstant CCharSequence
    deriving (Show)

newtype CCharSequence =
    CCharSequence (NonEmpty CChar)
    deriving (Show)

data CChar =
    SimpleCChar Char |
    EscapeSequence EscapeSequence
    deriving (Show)

data EscapeSequence =
    SimpleEscapeSequence SimpleEscapeSequence |
    OctalEscapeSequence OctalEscapeSequence |
    HexadecimalEscapeSequenceEscapeSequence HexadecimalEscapeSequence |
    UniversalCharacterName UniversalCharacterName
    deriving (Show)

data SimpleEscapeSequence =
    SingleQuote |
    DoubleQuote |
    QuestionMark |
    Backslash |
    Alert |
    Backspace |
    FormFeed |
    NewLine |
    CarriageReturn |
    HorizontalTab |
    VerticalTab
    deriving (Show)

data OctalEscapeSequence =
    SingleOctalEscapeSequence OctalDigit |
    DoubleOctalEscapeSequence OctalDigit OctalDigit |
    TripleOctalEscapeSequence OctalDigit OctalDigit OctalDigit
    deriving (Show)

newtype HexadecimalEscapeSequence =
    HexadecimalEscapeSequence (NonEmpty HexadecimalDigit)
    deriving (Show)

data UniversalCharacterName =
    ShortUniversalCharacterName HexQuad |
    LongUniversalCharacterName HexQuad HexQuad
    deriving (Show)

newtype HexQuad =
    HexQuad (HexadecimalDigit, HexadecimalDigit, HexadecimalDigit, HexadecimalDigit)
    deriving (Show)

data Expression =
    SimpleExpression AssignmentExpression |
    ComplexExpression Expression AssignmentExpression
    deriving (Show)

data GenericSelection =
    GenericSelection AssignmentExpression GenericAssocList
    deriving (Show)

newtype GenericAssocList =
    GenericAssocList (NonEmpty GenericAssociation)
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


