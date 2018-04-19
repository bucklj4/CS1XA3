module ExprTest where

import           ExprDiff
import           ExprParser
import           ExprPretty
import           ExprType
import           ExprGraphing
import           ExprBonus
import           Data.Fixed
import           Data.Maybe

import qualified Data.Map.Strict as Map
import           Test.QuickCheck

sampleExpr1 :: Expr Float
sampleExpr1 = (var "x") !+ (var "y")


listToExpr1 :: [Double] -> Expr Double
listToExpr1 [x]    = Const x
listToExpr1 (x:xs) = Add (Const x) (listToExpr1 xs)
listToExpr1 []     = error "Not list to expression for empty"


test1 :: Float -> Bool
test1 x = eval (Map.fromList [("x",x),("y",-x)]) sampleExpr1 == 0

test2 :: Double -> Bool
test2 x = let epsilon = 1e-5 in abs ((eval (Map.fromList [("x", x)]) (Log (Exp (Const x)))) - x) <= epsilon
