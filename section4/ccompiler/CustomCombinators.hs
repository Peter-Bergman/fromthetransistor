module CustomCombinators
( nullifyParser
, many1NonEmpty
, many1Till
, many1TillNonEmpty
, sepBy1NonConsumption
, tryMaybe
) where
import Data.List.NonEmpty
    ( fromList
    , NonEmpty
    )
import Text.Parsec.Combinator
    ( many1
    , manyTill
    , notFollowedBy
    , optionMaybe
    )
import Text.Parsec.Prim
    ( many
    , ParsecT
    , Stream
    , try
    , (<|>)
    )

sepBy1NonConsumption :: ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBy1NonConsumption parser separator = do
    firstItem <- parser
    followingItems <- many $ try $ separator >> parser
    return $ firstItem : followingItems

tryMaybe :: Stream s m t => ParsecT s u m a -> ParsecT s u m (Maybe a)
tryMaybe inputParser = try (optionMaybe inputParser) <|> return Nothing

many1NonEmpty :: Stream s m t => ParsecT s u m a -> ParsecT s u m (NonEmpty a)
many1NonEmpty parser = do
    parsedList <- many1 parser
    return $ fromList parsedList

many1Till :: Stream s m t => Show end => Show t => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
many1Till elementParser endParser = do
    notFollowedBy endParser
    -- Confirm that there is at least one element
    parsedHead <- elementParser
    parsedTail <- manyTill elementParser endParser
    return $ parsedHead : parsedTail

many1TillNonEmpty :: Stream s m t => Show end => Show t => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m (NonEmpty a)
many1TillNonEmpty elementParser endParser = do
    parsedList <- many1Till elementParser endParser
    return $ fromList parsedList

nullifyParser :: Stream s m t => ParsecT s u m a -> ParsecT s u m ()
nullifyParser parser = parser >> return ()

