module IncludeDirective where
import Lexer
    (lexC)
import NewLine
    (newLine)
import Octothorpe
    (octothorpe)
import PreprocessingProcessor
    ( PreprocessingParser
    , PreprocessingParserX
    , tryMaybe
    )
import HeaderName
    ( headerName
    , HeaderName
    )
import System.IO


includeDirective :: PreprocessingParser
includeDirective = do
    parsedMaybeHeaderName <- tryMaybe includeDirectiveInner
    case parsedMaybeHeaderName of
        Just parsedHeaderName -> handleIncludeDirective parsedHeaderName
        Nothing -> fail ""

includeDirectiveInner :: PreprocessingParserX HeaderName
includeDirectiveInner = do
    octothorpe
    include
    parsedHeaderName <- headerName
    newLine
    return parsedHeaderName

handleIncludeDirective :: HeaderName -> PreprocessingParser
handleIncludeDirective headerNameInput = let headerFileName = extractFileNameFromHeaderName headerNameInput in
    case headerFileName of
        HHeaderName fileName -> includeHHeaderName headerFileName
        QHeaderName fileName -> includeQHeaderName headerFileName

includeHSourceFile :: HeaderName -> PreprocessingParser
includeHSourceFile = do
    return ()

includeQSourceFile :: HeaderName -> PreprocessingParser
includeQSourceFile = do
    return ()

include :: PreprocessingParser
include = stringSatisfy (=="include")

