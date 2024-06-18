module DataDriven where
import Compiler.Compiler
import System.Directory
import Test.Tasty
import Test.Tasty.HUnit

determineAllTestCaseDirectories :: IO [String]
determineAllTestCaseDirectories = do
    putStrLn "determineAllTestDirectories running..."

    currentDirName <- getCurrentDirectory
    let testDataDirectoryName = currentDirName ++ "/test/test_data"

    testDataDirectoryContents <- listDirectoryContentsWithTheirPaths testDataDirectoryName
    print "test data test cases..."
    print testDataDirectoryContents

    let testCaseDirectories = testDataDirectoryContents
    return testCaseDirectories

buildTestTree :: IO TestTree
buildTestTree = do
    testCaseDirectories <- determineAllTestCaseDirectories
    testCases <- mapM buildTestCaseFromTestCaseDirectory testCaseDirectories
    let testTree = testGroup "Data Driven Tests" testCases
    return testTree

addPathToFileName :: FilePath -> FilePath -> FilePath
addPathToFileName pathName fileName = pathName ++ "/" ++ fileName

listDirectoryContentsWithTheirPaths :: FilePath -> IO [FilePath]
listDirectoryContentsWithTheirPaths directoryPath = do
    dirContentNames <- listDirectory directoryPath
    let dirContentPaths = map (addPathToFileName directoryPath) dirContentNames
    return dirContentPaths

inputFileName :: FilePath
inputFileName = "test_input.c"

expectedOutputFileName :: FilePath
expectedOutputFileName = "expected_output.S"

buildTestCaseFromTestCaseDirectory :: FilePath -> IO TestTree
buildTestCaseFromTestCaseDirectory testCaseDirectory = do
    let inputFilePath = addPathToFileName testCaseDirectory inputFileName
    inputForTest <- readFile inputFilePath
    let expectedOutputFilePath = addPathToFileName testCaseDirectory expectedOutputFileName
    expectedOutputForTest <- readFile expectedOutputFilePath
    let assertion = buildAssertionFromInputAndExpectedOutput inputForTest expectedOutputForTest testCaseDirectory
    let testCaseToReturn = testCase testCaseDirectory assertion
    return testCaseToReturn
    
    
buildAssertionFromInputAndExpectedOutput :: String -> String -> String -> Assertion
buildAssertionFromInputAndExpectedOutput input expectedOutput testName = assertEqual
    (testName ++ " - (Failed)") -- error message in case the assertion fails
    (compile input inputFileName) -- compiled input
    expectedOutput -- the expected value of the compiled input

