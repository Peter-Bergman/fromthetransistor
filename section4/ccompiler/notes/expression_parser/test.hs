module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec


main :: IO()
main = do
    args <- getArgs
    let firstArg = args !! 0
    putStrLn (readExpr (firstArg))


readExpr :: String -> String
readExpr input = case parse (pointerOperators 0) "pointers" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val




pointerOperators :: Integer -> Parser Integer
pointerOperators pointerLayers = do
    let pointerOperator = oneOf "&*"
    operator <- pointerOperator
    spaces
    let currentPointerLayers = pointerLayers + case operator of
            '&' -> 0-1
            '*' -> 0+1
    operatorsToTheRight <- pointerOperators currentPointerLayers
    return operatorsToTheRight
    <|>
    return pointerLayers
