module Group (group) where
import ControlLine
    (controlLine)
import Data.List
    ( intercalate
    , null
    )
import GroupPart
    (groupPart)
import NewLine
    (newLine)
import NonDirective
    (nonDirective)
import PreprocessingParser
    ( PreprocessingParser
    , stringParserSatisfy
    )
import Text.Parsec
    (many)
import Text.Parsec.Prim
    ( try
    , (<|>)
    , (<?>)
    )
import TextLine
    (textLine)


group :: PreprocessingParser
group = do
    parsedGroupPartsUnflattened <- many groupPart
    let parsedGroupParts = intercalate [] parsedGroupPartsUnflattened
    return parsedGroupParts

optionTry :: PreprocessingParser -> PreprocessingParser
optionTry = option [] $ try

optionalGroup :: PreprocessingParser
optionalGroup = optionTry group

optionalElifGroups :: PreprocessingParser
optionalElifGroups = optionTry elifGroups

optionalElseGroup :: PreprocessingParser
optionalElseGroup = optionTry elseGroup

groupPart :: PreprocessingParser
groupPart = 
    try (ifSection) <|>
    try (controlLine) <|>
    try (textLine) <|>
    nonDirective <?>
    "Group Part"

ifSection :: PreprocessingParser
ifSection = do
    parsedIfGroup <- ifGroup
    parsedElifGroups <- optionalElifGroups
    parsedElseGroup <- optionalElseGroup
    parsedEndIfLine <- endIfLine
    let fullIfSection = parsedIfGroup ++ parsedElifGroups ++ parsedElseGroup ++ parsedEndIfLine
    return fullIfSection

ifGroup :: PreprocessingParser
ifGroup = 
    try (ifDirective) <|>
    try (ifDefDirective) <|>
    ifNDefDirective

ifDirective :: PreprocessingParser
ifDirective = do
    octothorpe
    if_
    ifBody

ifDefDirective :: PreprocessingParser
ifDefDirective = do
    octothorpe
    ifDef
    ifDefBody True

ifNDefDirective :: PreprocessingParser
ifNDefDirective = do
    octothorpe
    ifNDef
    ifDefBody False

ifDefBody :: Bool -> PreprocessingParser
ifDefBody defOrNDef = do
    -- True corresponds to ifdef
    -- False corresponds to ifndef
    parsedIdentifier <- stringParserSatisfy identifier
    newLine
    parsedGroup <- optionalGroup
    -- get the user state
    currentState <- getState
    -- look up the parsedIdentifier in the state dictionary
    let isDefined = (lookup parsedIdentifier currentState) /= Nothing
    -- if identifier's existence or lackthereof corresponds with def/ndef,
    -- then keep the body and return parsedGroup;
    -- otherwise, nothing in the ifDefBody is kept in the
    -- translation unit
    let keepBody = defOrNDef == isDefined
    let tokensToReturn = if keepBody then parsedGroup else []
    return tokensToReturn

ifBody :: PreprocessingParser
ifBody = do
    parsedConstantExpression <- constantExpression
    newLine
    parsedGroup <- optionalGroup
    -- keepBody will be Bool equal to whether or not parsedConstantExpression is true or false
    let keepBody = evaluate parsedConstantExpression
    let tokensToReturn = if keepBody then parsedGroup else []
    return tokensToReturn

if_ :: PreprocessingParser
if_ = stringSatisfy (=="if")

ifDef :: PreprocessingParser
ifDef = stringSatisfy (=="ifdef")

ifNDef :: PreprocessingParser
ifNDef = stringSatisfy (=="ifndef")

elifGroups :: PreprocessingParser
elifGroups = do
    parsedElifGroupsUnflattened <- many elifGroup
    let :
    return elifGroupsFlattened


firstNonEmptyElementOrEmptyList :: Eq a => [[a]] -> [a]
firstNonEmptyElementOrEmptyList inputList =
    | null inputList = []
    | not $ null head_ = head_
    | otherwise = firstNonEmptyElementOrEmptyList tail_
    where
        head_ = head inputList
        tail_ = tail inputList

elifGroup :: PreprocessingParser
elifGroup = do
    parsedOctothorpe <- octothorpe
    parsedElif <- elif
    ifBody

elif :: PreprocessingParser
elif = stringSatisfy (=="elif")

elseGroup :: PreprocessingParser
elseGroup = do
    parsedOctothorpe <- octothorpe
    parsedElse <- else_
    parsedNewLine <- newLine
    parsedGroup <- optionalGroup

else_ :: PreprocessingParser
else_ = stringSatisfy (=="else")


