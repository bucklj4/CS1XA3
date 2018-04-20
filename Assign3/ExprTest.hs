module ExprTest where

import           ExprDiff
import           ExprParser
import           ExprPretty
import           ExprType
import           ExprGraphing
import           ExprBonus
import           Data.Fixed
import           Data.Maybe
import           Control.Exception
import           Test.QuickCheck
import qualified Data.Map.Strict as Map

sampleExpr1 :: Expr Float
sampleExpr1 = (var "x") !+ (var "y")


listToExpr1 :: [Double] -> Expr Double
listToExpr1 [x]    = Const x
listToExpr1 (x:xs) = Add (Const x) (listToExpr1 xs)
listToExpr1 []     = error "Not list to expression for empty"

listToExpr2 :: [Double] -> Expr Double
listToExpr2 [x]    = Const x
listToExpr2 (x:xs) = Mult (Const x) (listToExpr2 xs)
listToExpr2 []     = error "Not list to expression for empty"

epsilon = 1e-5

-- ******************************************************************
-- NOTE: Additional black-box test cases provided in README on GitHub
-- ******************************************************************

-- Adding numbers and then subtracting them should give a net result of 0
test1 :: Float -> Bool
test1 x = eval (Map.fromList [("x",x),("y",-x)]) sampleExpr1 == 0

-- The natural log of the natural exponential of anything is simply that anything. (Not the same in reverse as natural log's domain is undefined below 0)
test2 :: Double -> Bool
test2 x = abs ((eval (Map.fromList [("x", x)]) (Log (Exp (Const x)))) - x) <= epsilon

-- The derivative of sine is cylical. Every four derivatives, the original function is returned. Thus, a number passed to both should return the same result.
test3 :: Double -> Bool
test3 x = abs ((eval (Map.fromList[("x", x)]) $ partDiff "x" $ partDiff "x" $ partDiff "x" $ partDiff "x" $ parseExprD "sin(x)") - (eval (Map.fromList[("x", x)]) $ parseExprD "sin(x)")) <= epsilon

-- Pythagorean Identity
test4 :: Double -> Double -> Bool
test4 x y = abs (1 - ((eval (Map.fromList[("x", x)]) $ parseExprD "(sin(x))^2") + (eval (Map.fromList[("x", x)]) $ parseExprD "(cos(x))^2"))) <= epsilon

-- Testing Sum Encoding
test5 :: [Double] -> Bool
test5 [] = True
test5 xs = abs ((eval (Map.fromList []) $ listToExpr1 xs) - sum xs) <= epsilon

-- Testing Product Encoding (Using division because floating error builds up quickly)
test6 :: [Double] -> Bool
test6 [] = True
test6 xs = abs (1000 - ((eval (Map.fromList []) $ listToExpr2 xs) / product xs * 1000)) <= epsilon

-- Testing Sigma
test7 :: Integer  -> Bool
test7 x = let a = realToFrac(x) in abs ((eval (Map.fromList []) $ sigma "x" a (a+100) $ parseExprD "x") - sum[a, (a+1)..(a+100)]) <= epsilon

-- Gauss's summation formula for i
test8 :: Integer -> Bool
test8 x = let a = abs $ realToFrac(x) in abs ((eval (Map.fromList []) $ sigma "x" 1 a $ parseExprD "x") - (a*(a+1)/2)) <= epsilon

-- Gauss's summation formula for i^2
test9 :: Integer -> Bool
test9 x = let a = abs $ realToFrac(x) in abs ((eval (Map.fromList []) $ sigma "x" 1 a $ parseExprD "x^2") - (a*(a+1)*(2*a+1)/6)) <= epsilon

-- Gauss's summation formula for i^3
test10 :: Integer -> Bool
test10 x = let a = abs $ realToFrac(x) in abs ((eval (Map.fromList []) $ sigma "x" 1 a $ parseExprD "x^3") - (a^2*(a+1)^2/2^2)) <= epsilon



