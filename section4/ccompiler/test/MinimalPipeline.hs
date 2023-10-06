module Main where
import Compiler.Compiler
import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain testTree


testTree :: TestTree
testTree = testGroup "Tests" [minimalPipelineTest]

minimalPipelineTest :: TestTree
minimalPipelineTest = testCase "Minimal Pipeline" minimalPipelineAssertion

minimalPipelineAssertion :: Assertion
minimalPipelineAssertion = assertEqual "Minimal Pipeline Failed"
    (compile minimalPipelineInput "")
    minimalPipelineExpectedOutput

minimalPipelineInput :: String
minimalPipelineInput = "int x;\n\n"

minimalPipelineExpectedOutput :: String
minimalPipelineExpectedOutput = "\t.section bss\nx:\t.bytes 4"

