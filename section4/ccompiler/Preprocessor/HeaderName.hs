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
    | (head inputString) == '<' && (last inputString) == '>' = HHeaderName sourceFileName
    | (head inputString) == '"' && (last inputString) == '"' = QHeaderName sourceFileName
    | otherwise = error $ "Could not convert \"" ++ inputString ++ "\" to a HeaderName"
    where
        sourceFileName = extractFileNameFromHeaderNameString inputString

extractFileNameFromHeaderNameString :: String -> String
extractFileNameFromHeaderNameString headerNameString = init $ tail headerNameString

