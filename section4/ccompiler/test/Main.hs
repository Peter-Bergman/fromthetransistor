module Main where
import qualified DataDriven
import qualified MinimalPipeline
import Test.Tasty

main :: IO ()
main = do
    allTestTreesCombined <- combineAllTestTrees
    defaultMain allTestTreesCombined

combineAllTestTrees :: IO TestTree
combineAllTestTrees = do
    dataDrivenTestTree <- DataDriven.buildTestTree
    let minimalPipelineTestTree = MinimalPipeline.testTree
    let allTestTreesCombined = testGroup "All Tests" [dataDrivenTestTree, minimalPipelineTestTree]
    return allTestTreesCombined

