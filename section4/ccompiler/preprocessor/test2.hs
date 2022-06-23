module Main where

import System.Environment
import Text.ParserCombinators.Parsec

main :: IO()
main = do
    args <- getArgs
    let firstArg = args !! 0
    putStrLn(show a)
    putStrLn(show b)
    putStrLn(show c)


data Expr =
    Expr String |
    ParensExpr ParenthesesExpression deriving Show

data ParenthesesExpression =
    ExprInParens Expr deriving Show

x :: Expr
x = Expr "asdf"

y :: ParenthesesExpression
y = ExprInParens $ Expr "This is the value of y"

printExpression :: Expr -> String
printExpression (Expr expression) = expression
printExpression (ParensExpr expressionInParentheses) = printParenthesesExpression expressionInParentheses

printParenthesesExpression :: ParenthesesExpression -> String
printParenthesesExpression (ExprInParens expression) = printExpression expression

ifFunc :: Integer -> Integer
ifFunc x
    | x == 1 = 2
    | x == 2 = 4
    | otherwise = 0

a = ifFunc 1
b = ifFunc 2
c = ifFunc 5
