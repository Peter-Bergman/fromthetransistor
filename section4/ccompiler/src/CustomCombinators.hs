module CustomCombinators
( anyOf
, failsIfDoesNotConsumeAllInput
, nullifyParser
, many1NonEmpty
, many1Till
, many1TillNonEmpty
, parseADTAndConsumedInput
, sepBy1NonConsumption
, sepBy1NonConsumptionNonEmpty
, simpleExpression
, singleton
, tryMaybe
, tryWithFailMessage
) where
import Data.List
    ( stripPrefix )
import Data.List.NonEmpty
    ( fromList
    , NonEmpty
    )
import Text.Parsec.Combinator
    ( eof
    , many1
    , manyTill
    , notFollowedBy
    , optionMaybe
    )
import Text.Parsec.Prim
    ( getInput
    , many
    , ParsecT
    , parserFail
    , Stream
    , try
    , (<|>)
    , (<?>)
    )

sepBy1NonConsumption :: ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBy1NonConsumption parser separator = do
    firstItem <- parser
    followingItems <- many $ try $ separator >> parser
    return $ firstItem : followingItems

sepBy1NonConsumptionNonEmpty :: ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m (NonEmpty a)
sepBy1NonConsumptionNonEmpty parser separator = sepBy1NonConsumption parser separator >>= return . fromList

tryMaybe :: Stream s m t => ParsecT s u m a -> ParsecT s u m (Maybe a)
tryMaybe inputParser = try (optionMaybe inputParser) <|> return Nothing

many1NonEmpty :: Stream s m t => ParsecT s u m a -> ParsecT s u m (NonEmpty a)
many1NonEmpty parser = do
    parsedList <- many1 parser
    return $ fromList parsedList

many1Till :: Stream s m t => Show end => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
many1Till elementParser endParser = do
    notFollowedBy endParser
    -- Confirm that there is at least one element
    parsedHead <- elementParser
    parsedTail <- manyTill elementParser endParser
    return $ parsedHead : parsedTail

many1TillNonEmpty :: Stream s m t => Show end => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m (NonEmpty a)
many1TillNonEmpty elementParser endParser = do
    parsedList <- many1Till elementParser endParser
    return $ fromList parsedList

nullifyParser :: ParsecT s u m a -> ParsecT s u m ()
nullifyParser parser = parser >> return ()


anyOf :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m a
anyOf parserList = case parserList of
    [] -> parserFail "Expected parsers for anyOf"
    x:[] -> x
    x:xs -> (try x) <|> (anyOf xs)

failsIfDoesNotConsumeAllInput :: Stream s m t1 => Show t1 => ParsecT s u m t -> ParsecT s u m t
failsIfDoesNotConsumeAllInput parser = do
    parsedData <- parser
    eof
    return parsedData

tryWithFailMessage :: String -> ParsecT s u m a -> ParsecT s u m a
tryWithFailMessage failMessage parser = try parser <?> failMessage

simpleExpression :: ParsecT s u m isomorphicType1 -> (isomorphicType1 -> isomorphicType2) -> ParsecT s u m isomorphicType2
simpleExpression initialParser constructor = try $ initialParser >>= return . constructor

singleton :: ParsecT s u m a -> ParsecT s u m [a]
singleton parser = do
    parsedSingleElement <- parser
    return [parsedSingleElement]

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix a b = reverse <$> stripPrefix (reverse a) (reverse b)

-- This combinator only works for list streams.
-- I doubt I will need to use any other kind of stream.
parseADTAndConsumedInput :: Monad m => Eq t => ParsecT [t] u m a -> ParsecT [t] u m ([t],a)
parseADTAndConsumedInput adtParser = do
    inputStreamBeforeConsumption <- getInput
    parsedADT <- adtParser
    inputStreamAfterConsumption <- getInput
    let consumedInput = determineConsumedInput inputStreamAfterConsumption inputStreamBeforeConsumption
    return (consumedInput, parsedADT)
    where
        emptyStream = []
        determineConsumedInput inputStreamAfterConsumption inputStreamBeforeConsumption =
            case stripSuffix inputStreamAfterConsumption inputStreamBeforeConsumption of
                Nothing -> emptyStream
                Just consumedInput' -> consumedInput'

