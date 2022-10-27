module HeaderName (headerName) where
import PreprocessingParser
    ( PreprocessingParserX
    , stringParserSatisfy
    )
import qualified CharTokenParsers.HeaderNames.HeaderName
    ( headerName
    )

headerName :: PreprocessingParserX String
headerName = stringParserSatisfy HeaderName.headerName

