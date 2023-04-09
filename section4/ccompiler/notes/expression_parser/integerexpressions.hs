integerExpression = do
    ternaryConditionExpression

{-
logicalExpression = do
    anyComparison
    logicalOperator
    logicalExpression
    <|>
    anyComparison
-}

ternaryConditionExpression = do
    condition <- logicalOrExpression
    char '?'
    thenResult <- ternaryConditionExpression
    char ':'
    elseResult <- ternaryConditionExpression
    <|>
    logicalOrExpression

logicalOrExpression = do
    firstSubExpression <- logicalAndExpression
    string "||"
    restOfExpression <- logicalOrExpression
    <|>
    logicalAndExpression

logicalAndExpression = do
    firstSubExpression <- bitwiseOrExpression
    string "&&"
    restOfExpression <- logicalAndExpression
    <|>
    bitwiseOrExpression

bitwiseOrExpression = do
    firstSubExpression <- bitwiseXorExpression
    string "|"
    restOfExpression <- bitwiseOrExpression
    <|>
    bitwiseXorExpression

bitwiseXorExpression = do
    firstSubExpression <- bitwiseAndExpression
    string "^"
    restOfExpression <- bitwiseXorExpression
    <|>
    bitwiseAndExpression

bitwiseAndExpression = do
    firstSubExpression <- equalityExpression
    char '&'
    restOfExpression <- bitwiseAndExpression
    <|>
    equalityExpression

{-
anyComparison = do
    equalityExpression
    <|> comparison
    <|> token (oneOf "10")
-}

equalityExpression = do
    firstSubExpression <- comparison
    operator <- equalityOperator
    restOfExpression <- equalityExpression
    <|>
    comparison

{-
nonEqualityExpression = do
    comparison
    <|>
    terms
-}

comparison = do
    firstSubExpression <- shiftExpression
    spaces
    operator <- comparisonOperator
    spaces
    restOfExpression <- comparison
    <|>
    shiftExpression

shiftExpression = do
    firstSubExpression <- terms
    spaces
    operator <- shiftOperator
    spaces
    restOfExpression <- shiftExpression
    <|>
    terms

terms = do
    firstSubExpression <- factors
    spaces
    operator <- additionOperator
    spaces
    restOfExpression <- terms
    <|>
    factors

factors = do
    firstSubExpression <- bitwiseExpression
    operator <- multiplicationOperator
    restOfExpression <- factors
    <|>
    bitwiseExpression

{-
bitwiseExpression = do
    unary
    bitwiseOperator
    bitwiseExpression
    <|>
    unary
-}


{-
unary = do
    unaryOperators ' ' -- This definition hill need more review. The - sign cannot be used next to itself.
    cast <- castOperators
    pointerLayers <- pointerOperators -- Just noticed that this is only acceptable if the primary is a variable
    primary
-}
unary = do
    unaryOperator
    unary
    <|>
    primary

primary = do
    constant
    <|> variable
    <|> functionCall
    <|> parens (intExpression)
    <|> compoundLiteral







{-
logicalOperator :: Parser String
logicalOperator = do
    string "&&"
    <|>
    string "||"
-}

comparisonOperator :: Parser String
comparisonOperator = do
    string "<"
    <|> string ">"
    <|> string "<="
    <|> string ">="

shiftOperator :: Parser String
shiftOperator = do
    string "<<"
    <|> string ">>"
    

additionOperator :: Parser Char
additionOperator = char '+' <|> char '-'

multiplicationOperator :: Parser String
multiplicationOperator = string "*" <|> string "/" <|> string "//" <|> string "%"

{-
unaryOperators lastCharacter = do
    let acceptableOperators lastChar = case lastChar of
        '-' -> "!~"
        _ -> "!~-"
    let currentOperator = oneOf $ acceptableOperators lastCharacter
    op <- try currentOperator
    spaces
    opsToRight <- unaryOperators op
    return [op] ++ opsToRight
    <|>
    string ""
-- The unaryOperators definition above needs revision
-}
unaryOperator = do
    string "++"
    string "--"
    string "+"
    string "-"
    string "!"
    string "~"
    parseCast
    string "*"
    string "&"
    --sizeof function call
    --_Alignof function call

pointerOperators :: Integer -> Parser Integer
pointerOperators pointerLayers = do
    let pointerOperator = oneOf "&*"
    operator <- pointerOperator
    spaces
    let currentPointerLayers = pointerLayers + case operator of
            '&' -> 0-1
            '*' -> 0+1
    operatorsToTheRight <- pointerOperators currentPointerLayers
    return operatorsToTheRight
    <|>
    return pointerLayers


castOperators = do
    let parseCast = parens parseType
    finalCast <- parseCast
    spaces
    castOperators
    return finalCast
    <|>
    return ""

