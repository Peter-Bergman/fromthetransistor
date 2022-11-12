module ControlLine (controlLine) where
import Data.List
    (intercalate)
import HeaderName
    ( HeaderName
    , headerName
    )
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
import PPTokens
    (ppTokens)
import PreprocessingParser
    ( anyStringToken
    , PreprocessingParser
    , PreprocessingParserX
    , stringSatisfy
    , stringParserSatisfy
    , tryMaybe
    )
import ReplacementList
    (replacementList)
import Text.Parsec 
    (string)
import Text.Parsec.Combinator
    ( option
    , optionMaybe
    )
import Text.Parsec.Prim
    ( getInput
    , setInput
    , try
    , (<|>)
    , (<?>)
    )
import Text.Parsec.String

controlLine :: PreprocessingParser
controlLine = 
    includeDirective <|>
    try (defineDirective) <|>
    try (undefDirective) <|>
    try (lineDirective) <|>
    errorDirective <|>
    pragmaDirective <|>
    try (nullDirective) <?>
    "Control Line"

-- could left factor this at some point
-- would like to think of good variable names before doing that
defineDirective :: PreprocessingParser
defineDirective = 
    try (objectLikeMacroDefinition) <|> functionLikeMacroDefinition

objectLikeMacroDefinition :: PreprocessingParser
objectLikeMacroDefinition = do
    parsedDefinePrefix <- definePrefix
    parsedIdentifier <- stringParserSatisfy identifier
    parsedReplacementList <- replacementList
    parsedNewLine <- newLine
    -- see if macro is already in user state dictionary
    -- update the user state dictionary if necessary
    return $ parsedDefinePrefix ++ parsedIdentifier ++ 
        parsedReplacementList ++ parsedNewLine

functionLikeMacroDefinition :: PreprocessingParser
functionLikeMacroDefinition = do
    parsedDefinePrefix <- definePrefix
    parsedIdentifier <- stringParserSatisfy identifier
    {- 
        The anyStrinToken parser used below accepts any string
        We are using it simply to feed its output into
        defineDirectiveArgumentList
        That being said, if it does not parse white-space,
        defineDirectiveArgumentList on the line following 
        will fail.
    -}
    parsedWhiteSpace <- anyStringToken
    let parsedWhiteSpaceAsString = head parsedWhiteSpace
    parsedDefineArgumentList <- 
        defineDirectiveArgumentList parsedWhiteSpaceAsString
    parsedReplacementList <- replacementList
    parsedNewLine <- newLine
    handleArgumentListAndReplacementList
    -- I might not include the parsedWhiteSpace in the 
    -- return statement in the future
    return $ parsedDefinePrefix ++ parsedIdentifier ++
        parsedWhiteSpace ++ parsedDefineArgumentList ++
        parsedReplacementList ++ parsedNewLine
    where
        -- see if macro is already in user state dictionary
        -- update the user state dictionary if necessary
        handleArgumentListAndReplacementList = return ()

defineDirectiveArgumentList :: String -> PreprocessingParser
defineDirectiveArgumentList precedingString = do
    parsedLParen <- lParen precedingString
    parsedIdentifierList <- argumentList
    parsedRParen <- stringSatisfy (==")")
    return $ parsedLParen ++ parsedIdentifierList ++ parsedRParen
            
argumentList :: PreprocessingParser
argumentList = 
    try (complexArguments) <|>
    ellipsis <|>
    simpleArguments
    
complexArguments :: PreprocessingParser
complexArguments = do
    parsedSimpleArguments <- simpleArguments
    gotArguments parsedSimpleArguments
    parsedComma <- stringSatisfy (==",")
    parsedEllipsis <- ellipsis
    return $ 
        parsedSimpleArguments ++ parsedComma ++ parsedEllipsis
    where
        gotArguments parsedArgumentList = if parsedArgumentList == []
            then fail "Expected argument list"
            else return ()

simpleArguments :: PreprocessingParser
simpleArguments = option [""] identifierList

ellipsis :: PreprocessingParser
ellipsis = stringSatisfy (=="...")

definePrefix :: PreprocessingParser
definePrefix = do
    parsedOctothorpe <- octothorpe
    parsedDefine <- define
    return $ parsedOctothorpe ++ parsedDefine

define :: PreprocessingParser
define = stringParserSatisfy $ string "define"

undefDirective :: PreprocessingParser
undefDirective = do
    parsedOctothorpe <- octothorpe
    parsedUndef <- stringSatisfy $ (=="undef")
    parsedIdentifier <- stringParserSatisfy identifier
    parsedNewLine <- newLine
    undefineIdentifier
    return $ parsedOctothorpe ++ parsedUndef ++ 
        parsedIdentifier ++ parsedNewLine
    where
        -- will need to define undefineIdentifier in the future
        undefineIdentifier = return ()

lineDirective :: PreprocessingParser
lineDirective = do
    parsedMaybeLineDirectiveTokens <- tryMaybe lineDirectiveInner
    case parsedMaybeLineDirectiveTokens of
        Just tokens -> handleLineDirective
        Nothing -> fail ""

lineDirectiveInner :: PreprocessingParser
lineDirectiveInner = do
    octothorpe
    stringSatisfy $ (=="line")
    parsedPPTokens <- ppTokens
    newLine
    return parsedPPTokens

handleLineDirective :: [String] -> PreprocessingParser
handleLineDirective = do
    -- checkback and implement
    return ()

handleLineDirective :: [String] -> PreprocessingParser
handleLineDirective tokens = do
    let numTokens = length tokens
    case numTokens of
        [] -> fail "fuck you; you gave no preprocessing tokens to process"
        x:[] -> fail "..."
        x:y:[] -> fail "..."
        x:xs -> fail "..."
    return ()

errorDirective :: PreprocessingParser
errorDirective = do
    parsedMaybeErrorDirectiveTokens <- tryMaybe errorDirectiveInner
    -- This should throw an error that will
    -- propagate through any parsers constructed with try
    -- in the future
    case parsedMaybeErrorDirectiveTokens of
        Just tokens -> handleErrorDirective tokens
        Nothing -> fail "" -- This fail is a fail that does not consume any input and thus can be <|>ed out of without try

errorDirectiveInner :: PreprocessingParser
errorDirectiveInner = do
    octothorpe
    error_
    parsedPPTokens <- option [] ppTokens 
    newLine
    return parsedPPTokens


handleErrorDirective :: [String] -> PreprocessingParser
handleErrorDirective tokens = fail tokensCombined
    where
        tokensCombined = intercalate " " tokens

error_ :: PreprocessingParser
error_ = stringSatisfy (=="error")

pragmaDirective :: PreprocessingParser
pragmaDirective = do
    parsedMaybePragmaDirectiveTokens <- tryMaybe pragmaDirectiveInner
    case parsedMaybePragmaDirectiveTokens of
        Just tokens -> handlePragmaDirective tokens
        Nothing -> fail ""

pragmaDirectiveInner :: PreprocessingParser
pragmaDirectiveInner = do
    octothorpe
    pragma
    parsedPPTokens <- ppTokens
    newLine
    return parsedPPTokens

-- The implementation below may be temporary. The type signature is will stay the same, though.
handlePragmaDirective :: [String] -> PreprocessingParser
handlePragmaDirective _ = handleErrorDirective ["Pragma Directive...throwing error cause I'm not implementing more shit"]

pragma :: PreprocessingParser
pragma = stringSatisfy (=="pragma")

 
nullDirective :: PreprocessingParser
nullDirective = octothorpe >> newLine

