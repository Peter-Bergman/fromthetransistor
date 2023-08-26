module Preprocessor.Group (group) where
import AbstractSyntaxTree
    ( ConstantExpression
        (ConstantExpression)
    , ElifGroup
        (ElifGroup)
    , ElifGroups
        (ElifGroups)
    , ElseGroup
        (ElseGroup)
    , Group
        (Group)
    , GroupPart
        ( ControlLine
        , IfSection
        )
    , IfGroup
        ( IfDefDirective
        , IfDirective
        , IfNDefDirective
        )
    )
import CustomCombinators
    (simpleExpression)
import Preprocessor.ControlLine
    (controlLine)
import CustomCombinators
    ( nullifyParser
    , many1NonEmpty
    , many1TillNonEmpty
    , tryMaybe
    )
import Preprocessor.EndIfLine
    (endIfLine)
import Preprocessor.Identifier
    (identifier)
import Preprocessor.NewLine
    (newLine)
import Preprocessor.NonDirectiveLine
    (nonDirectiveLine)
import Preprocessor.Octothorpe
    (octothorpe)
import Preprocessor.PreprocessingParser
    ( PreprocessingParser
    , PreprocessingParserX
    , stringParserSatisfy
    , stringSatisfy_
    )
import Text.Parsec.Char
    (digit)
import Text.Parsec.Combinator
    ( eof
    , lookAhead
    , many1
    )
import Text.Parsec.Prim
    ( parserFail
    , try
    , (<|>)
    , (<?>)
    )
import Preprocessor.TextLine
    (textLine)


data ContainingGroup =
    IfOrElif |
    Else |
    None

group :: ContainingGroup -> PreprocessingParserX Group
group containingGroup = simpleExpression (many1TillNonEmpty (try groupPart) $ lookAhead $ endOfInnerGroup containingGroup) Group

groupPart :: PreprocessingParserX GroupPart
groupPart = 
        try (ifSection) <|>
        try (controlLineGroupPart) <|>
        try (nonDirectiveLine) <|>
        textLine <?>
        "Group Part"

endOfInnerGroup :: ContainingGroup -> PreprocessingParserX ()
endOfInnerGroup containingGroup = case containingGroup of
    IfOrElif -> try (nullifyParser elifGroup) <|> try (nullifyParser elseGroup) <|> nullifyParser endIfLine
    Else -> nullifyParser endIfLine
    None -> eof

controlLineGroupPart :: PreprocessingParserX GroupPart
controlLineGroupPart = controlLine >>= return . ControlLine

ifSection :: PreprocessingParserX GroupPart
ifSection = (parserFail "not implemented or implementation not used") {-do
    parsedIfGroup <- ifGroup
    parsedElifGroups <- tryMaybe elifGroups
    parsedElseGroup <- tryMaybe elseGroup
    -- parsed elseGroup
    endIfLine
    return $ IfSection parsedIfGroup parsedElifGroups parsedElseGroup
-}

ifGroup :: PreprocessingParserX IfGroup
ifGroup =
    try (ifDirective) <|>
    try (ifDefDirective) <|>
    ifNDefDirective

ifDirective :: PreprocessingParserX IfGroup
ifDirective = (parserFail "not implemented or implementation not used") {-do
    ifPrefix
    parsedConstantExpression <- constantExpression
    newLine
    parsedMaybeGroup <- tryMaybe $ group IfOrElif
    return $ IfDirective parsedConstantExpression parsedMaybeGroup
-}

ifDefDirective :: PreprocessingParserX IfGroup
ifDefDirective = do
    ifDefPrefix
    parsedIdentifier <- identifier
    newLine
    parsedMaybeGroup <- tryMaybe $ group IfOrElif
    return $ IfDefDirective parsedIdentifier parsedMaybeGroup

ifNDefDirective :: PreprocessingParserX IfGroup
ifNDefDirective = do
    ifNDefPrefix
    parsedIdentifier <- identifier
    newLine
    parsedMaybeGroup <- tryMaybe $ group IfOrElif
    return $ IfNDefDirective parsedIdentifier parsedMaybeGroup

elifGroups :: PreprocessingParserX ElifGroups
elifGroups = simpleExpression (many1NonEmpty $ try elifGroup) ElifGroups

ifPrefix :: PreprocessingParserX ()
ifPrefix = octothorpe >> if_

if_ :: PreprocessingParserX ()
if_ = stringSatisfy_ (=="if")

ifDefPrefix :: PreprocessingParserX ()
ifDefPrefix = octothorpe >> ifDef

ifDef :: PreprocessingParserX ()
ifDef = stringSatisfy_ (=="ifdef")

ifNDefPrefix :: PreprocessingParserX ()
ifNDefPrefix = octothorpe >> ifNDef

ifNDef :: PreprocessingParserX ()
ifNDef = stringSatisfy_ (=="ifndef")

elifGroup :: PreprocessingParserX ElifGroup
elifGroup = do
    elifPrefix
    parsedConstantExpression <- constantExpression
    newLine
    parsedMaybeGroup <- tryMaybe $ group IfOrElif
    return $ ElifGroup parsedConstantExpression parsedMaybeGroup

elifPrefix :: PreprocessingParserX ()
elifPrefix = octothorpe >> elif

elif :: PreprocessingParserX ()
elif = stringSatisfy_ (=="elif")

elseGroup :: PreprocessingParserX ElseGroup
elseGroup = do
    elsePrefix
    newLine
    parsedGroup <- tryMaybe $ group Else
    return $ ElseGroup parsedGroup

elsePrefix :: PreprocessingParserX ()
elsePrefix = octothorpe >> else_

else_ :: PreprocessingParserX ()
else_ = stringSatisfy_ (=="else")

-- NOTE: This definition is just for the sake of being able to build this module and test other parsers.
-- FIXME: THEY DO NOT REFLECT THE ACTUAL SYNTAX OF A CONSTANT EXPRESSION
constantExpression :: PreprocessingParserX ConstantExpression
constantExpression = (parserFail "not implemented or implementation not used") {-try $ do
    parsedDigits <- stringParserSatisfy (many1 digit)
    let digitsAsInteger = ((read :: String -> Integer) . head) parsedDigits
    --let digitsAsConditionalExpression = SimpleConditionalExpression . LogicalOrExpression . 
    return $ ConditionalExpressionConstantExpression digitsAsInteger
-}
