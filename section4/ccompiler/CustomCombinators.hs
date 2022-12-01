module CustomCombinators
( sepBy1NonConsumption
, tryMaybe
) where
import Data.List.NonEmpty
    ( fromList
    , NonEmpty
    )
import Text.Parsec.Combinator
    ( many1
    , optionMaybe
    )
import Text.Parsec.Prim
    ( many
    , Parsec
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

