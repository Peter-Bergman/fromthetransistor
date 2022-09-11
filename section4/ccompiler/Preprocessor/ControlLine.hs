module ControlLine (controlLine) where
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
    , stringSatisfy
    , stringParserSatisfy
    )
import ReplacementList
    (replacementList)
import Text.Parsec 
    (string)
import Text.Parsec.Combinator
    (option)
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
    try (includeDirective) <|>
    try (defineDirective) <|>
    try (undefDirective) <|>
    try (lineDirective) <|>
    try (errorDirective) <|>
    try (pragmaDirective) <|>
    nullDirective <?>
    "Control Line"

includeDirective :: PreprocessingParser
includeDirective = do
    parsedOctothorpe <- octothorpe
    parsedInclude <- include
    parsedNewLine <- newLine
    includeFile
    return $ parsedOctothorpe ++ parsedInclude ++ parsedNewLine
    where
        -- find and lex the included file
        -- update stream to include lexed stream from included file
        -- use getInput and setInput functions from Text.Parsec.Prim
        includeFile = return ()

include :: PreprocessingParser
include = stringSatisfy (=="include")
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
    where
        argumentList = 
            try (complexArguments) <|>
            ellipsis <|>
            simpleArguments
        simpleArguments = option [] identifierList
        ellipsis = stringSatisfy (=="...")
        complexArguments = do
            parsedSimpleArguments <- simpleArguments
            gotArguments parsedSimpleArguments
            parsedComma <- stringSatisfy (==",")
            parsedEllipsis <- ellipsis
            return $ 
                parsedSimpleArguments ++ parsedComma ++ parsedEllipsis
        gotArguments parsedArgumentList = if parsedArgumentList == []
            then fail "Expected argument list"
            else return ()
            
    
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
    parsedOctothorpe <- octothorpe
    parsedLine <- stringSatisfy $ (=="line")
    parsedPPTokens <- ppTokens
    parsedNewLine <- newLine
    handlePPTokens
    return $ 
        parsedOctothorpe ++ parsedLine ++ parsedPPTokens
    where
        -- should change line or something in the future
        handlePPTokens = return ()

errorDirective :: PreprocessingParser
errorDirective = do
    parsedOctothorpe <- octothorpe
    parsedError <- stringSatisfy (=="error")
    parsedPPTokens <- ppTokens
    handleTokens
    return $ 
        parsedOctothorpe ++ parsedError ++ parsedPPTokens
    where
        -- This should throw an error that will
        -- propagate through any parsers constructed with try
        -- in the future
        handleTokens = return ()

pragmaDirective :: PreprocessingParser
pragmaDirective = do
    parsedOctothorpe <- octothorpe
    parsedPragma <- stringSatisfy (=="pragma")
    parsedPPTokens <- ppTokens
    parsedNewLine <- newLine
    handlePPTokens parsedPPTokens
    return $ parsedOctothorpe ++ parsedPragma ++ parsedPPTokens ++
        parsedNewLine
    where
        -- might decide to add some implementation specific 
        -- functionality in the future
        handlePPTokens tokens = return ()
    
nullDirective :: PreprocessingParser
nullDirective = octothorpe >> newLine

