module HeaderName (headerName, HeaderName) where
import PreprocessingParser
    ( PreprocessingParserX
    , stringParserSatisfyT
    )
import qualified CharTokenParsers.HeaderNames.HeaderName
    ( headerName
    )

headerName :: PreprocessingParserX HeaderName
headerName = stringParserSatisfyT CharTokenParsers.HeaderNames.HeaderName.headerName headerNameStringToHeaderName

data HeaderName = 
    HHeaderName String |
    QHeaderName String

headerNameStringToHeaderName :: String -> HeaderName
headerNameStringToHeaderName inputString
    | head inputString == '>' = HHeaderName inputString
    | head inputString == '"' = QHeaderName inputString
    | otherwise = error "Could not convert \"" ++ inputString ++ "\" to a HeaderName"

