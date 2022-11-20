module CustomCombinators where
--import Data.Stream
--    (Stream)
import Text.Parsec.Prim
    ( many
    , ParsecT
    , try
    )

sepBy1NonConsumption :: ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBy1NonConsumption parser separator = do
    firstItem <- parser
    followingItems <- many $ try $ separator >> parser
    return $ firstItem : followingItems

