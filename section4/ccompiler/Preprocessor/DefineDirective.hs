module DefineDirective (defineDirective) where
import AbstractSyntaxTree --importing all the types from AbstractSyntaxTree
    ( ControlLine
        (DefineDirective)
    , DefineDirective
        ( ObjectLikeDefine
        , FunctionDefine
        , EllipsisFunctionDefine
        , IdentifierListEllipsisFunctionDefine
        )
    , Identifier
    , IdentifierList
    , ReplacementList
    )
import Comma
    (comma)
import Data.Char
    (isSpace)
import IdentifierList
    (identifierList)
import Lexer.PreprocessingToken
    (identifier)
import LParen
    (lParen)
import NewLine
    (newLine)
import Octothorpe
    (octothorpe)
import PreprocessingParser
    ( PreprocessingParser
    , PreprocessingParserX
    , stringParserSatisfy
    , stringParserSatisfyT
    , stringSatisfy_
    , stringSatisfyT
    )
import ReplacementList
    (replacementList)
import Text.Parsec
    (string)
import Text.Parsec.Combinator
    ( between
    , optionMaybe
    )
import Text.Parsec.Prim
    ( try
    , (<|>)
    , (<?>)
    )

defineDirective :: PreprocessingParserX ControlLine
defineDirective = defineDirectiveProper >>= return . DefineDirective

defineDirectiveProper :: PreprocessingParserX DefineDirective
defineDirectiveProper = between definePrefix newLine defineDirectiveBody

defineDirectiveBody :: PreprocessingParserX DefineDirective
defineDirectiveBody = do
    let parseErrorMessage = "Define Directive Body"
    parsedIdentifier <- stringParserSatisfyT identifier id <?> parseErrorMessage
    -- Order matters here. If I put objectLikeMacroDefinitionBody first, then a function-like macro definition would be parsed as an object like macro definition.
    try (functionLikeMacroDefinitionBody parsedIdentifier) <|> (objectLikeMacroDefinitionBody parsedIdentifier) <?> parseErrorMessage

objectLikeMacroDefinitionBody :: Identifier -> PreprocessingParserX DefineDirective
objectLikeMacroDefinitionBody parsedIdentifier = do
    parsedReplacementList <- replacementList
    return $ ObjectLikeDefine parsedIdentifier parsedReplacementList

functionLikeMacroDefinitionBody :: Identifier -> PreprocessingParserX DefineDirective
functionLikeMacroDefinitionBody parsedIdentifier = do
    lParen
    functionLikeMacroDefinitionSuffix parsedIdentifier

data DefineDirectiveArgumentList =
    MaybeIdentifierList (Maybe IdentifierList) |
    EllipsisArgumentList |
    IdentifierArgumentList IdentifierList
    deriving (Show)

functionLikeMacroDefinitionSuffix :: Identifier -> PreprocessingParserX DefineDirective
functionLikeMacroDefinitionSuffix parsedIdentifier = do
    parsedDefineDirectiveArgumentList <- functionLikeMacroDefinitionArgumentList
    rParen
    parsedReplacementList <- replacementList
    return $ constructDefineDirective parsedIdentifier parsedDefineDirectiveArgumentList parsedReplacementList

constructDefineDirective :: Identifier -> DefineDirectiveArgumentList -> ReplacementList -> DefineDirective
constructDefineDirective parsedIdentifier parsedDefineDirectiveArgumentList parsedReplacementList = case parsedDefineDirectiveArgumentList of
         MaybeIdentifierList parsedMaybeIdentifierList -> FunctionDefine parsedIdentifier parsedMaybeIdentifierList parsedReplacementList
         EllipsisArgumentList -> EllipsisFunctionDefine parsedIdentifier parsedReplacementList
         IdentifierArgumentList parsedIdentifierList -> IdentifierListEllipsisFunctionDefine parsedIdentifier parsedIdentifierList parsedReplacementList

functionLikeMacroDefinitionArgumentList :: PreprocessingParserX DefineDirectiveArgumentList
functionLikeMacroDefinitionArgumentList =
    try (identifierArgumentList) <|>
    try (ellipsisArgumentList) <|>
    maybeIdentifierList

ellipsisArgumentList :: PreprocessingParserX DefineDirectiveArgumentList
ellipsisArgumentList = stringSatisfyT (=="...") (\_ -> EllipsisArgumentList)

maybeIdentifierList :: PreprocessingParserX DefineDirectiveArgumentList
maybeIdentifierList = do
    parsedIdentifierList <- optionMaybe identifierList
    return $ MaybeIdentifierList parsedIdentifierList

identifierArgumentList :: PreprocessingParserX DefineDirectiveArgumentList
identifierArgumentList = do
    parsedIdentifierList <- identifierList
    comma
    ellipsis
    return $ IdentifierArgumentList parsedIdentifierList

noWhiteSpaceString :: PreprocessingParserX ()
noWhiteSpaceString = stringSatisfy_ $ isSpacingInString

isSpacingInString :: String -> Bool
isSpacingInString = any isSpace

definePrefix :: PreprocessingParserX ()
definePrefix = octothorpe >> define

-- Here are all of the simple parsers whose condition is simply a string comparison

ellipsis :: PreprocessingParserX ()
ellipsis = stringSatisfy_ (=="...")

rParen :: PreprocessingParserX ()
rParen = stringSatisfy_ (==")")

define :: PreprocessingParserX ()
define = stringSatisfy_ $ (=="define")

