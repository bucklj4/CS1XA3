{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}

module ExprBonus where

import           ExprType
import           ExprDiff
import           UnsafeToPure
import           Data.List
import           Data.Fixed
import           Data.Maybe


import qualified Data.Map.Strict as Map

class DiffExprBonus a where
  pureEval :: Map.Map String a -> Expr a -> Maybe a
  pureSimplify :: Map.Map String a -> Expr a -> Maybe (Expr a)

  sigma :: String -> a -> a -> Expr a -> Expr a
  isConverging :: String -> a -> Expr a -> Bool
  integral ::  String -> a -> a -> a -> Expr a -> Expr a
  newton :: String -> a -> a -> Expr a -> Expr a
  criticalPoints :: String -> a -> a -> Expr a -> [(Expr a)]
  extrema :: String -> a -> a -> Expr a -> [(String, (a, a))]
  fixFloatingError :: Expr a -> Expr (a)
  pureNewton :: String -> a -> a -> Expr a -> Maybe (Expr a)

instance (Num a, Eq a, Fractional a, Floating a, Ord a, Show a, RealFrac a) => DiffExprBonus a where
  pureEval vrs expr = unsafeCleanup $ eval vrs expr
  pureSimplify vrs expr = unsafeCleanup $ simplify vrs expr

  sigma x 0 0 expr = (Const 0)
  sigma x a b expr = let q = reduce expr 
                      in 
                      case (length $ getVars expr) of 
                      0 -> if a == b then (Const (eval (Map.fromList []) q)) else if b < a then (Const 0) else (Const (eval (Map.fromList [(x, b)]) q) !+ (sigma x a (b-1) q))
                      1 -> if a == b then (Const (eval (Map.fromList [(x, a)]) q)) else if b < a then (Const 0) else (Const (eval (Map.fromList [(x, b)]) q) !+ (sigma x a (b-1) q))
                      _ -> error "You have more than one independent variable in the expression!"

  isConverging x a expr = case (length $ getVars expr) of 
                          0 -> False
                          1 -> (abs((eval (Map.fromList [(x, a)]) $ sigma x a (100000) expr) / (eval (Map.fromList [(x, a)]) $ sigma x a (1000) expr) -1)*100) <= 35
                          _ -> error "You have more than one independent variable in the expression!"

  integral x a b n expr = let 
                        deltaX = (b-a)/n
                        doIntegral x a b expr deltaX = if a == b then (Const 0) else if a > b then (Const 0) else (simplify (Map.fromList [(x, a)]) expr) !+ (doIntegral x (a+deltaX) b expr deltaX) 
                        in
                        case (length $ getVars expr) of
                          0       | a < b -> (Const deltaX) !* (doIntegral x a b expr deltaX)
                                  | otherwise -> (Const (deltaX)) !* (doIntegral x b a expr (-deltaX))

                          1       | x /= (head $ getVars expr) -> error "The variable to which is respect is given does not appear in the expression!"
                                  | a < b -> (Const deltaX) !* (doIntegral x a b expr deltaX)
                                  | otherwise -> (Const (deltaX)) !* (doIntegral x b a expr (-deltaX))

                          _ -> error "You have more than one independent variable in the expression!"

  

  
  newton x s maxx p = case (length $ getVars p) of
                      0 -> error "You have no independent variable  in the expression!"
                      1     | x /= (head $ getVars p) -> error "The variable to which is respect is given does not appear in the expression!"
                            | maxx == 0 -> error ("Newton's Method was unable to find a root after the maximum number of iterations allowed using the given initial guess. Please make a more accurate guess or increase the maximum recursion limit and try again.")
                            | (eval (Map.fromList [(x, s)]) $ partDiff x p) == 0 -> error "The deriative at this point is 0!" 
                            | abs (eval (Map.fromList [(x, s)]) p) <= (0 + 1e-10) -> (Const s) 
                            | otherwise -> newton x (s - (eval (Map.fromList [(x, s)]) p) / (eval (Map.fromList [(x, s)]) $ partDiff x p)) (maxx - 1) p
                      _ -> error "You have more than one independent variable in the expression!"  


  criticalPoints x a b expr = let  
                              doCriticalPoints x a b expr   | a >= b =  let q = reduce expr in [pureNewton x a 100 $ partDiff x q] 
                                                            | a < b = let q = reduce expr in [pureNewton x b 100 $ partDiff x q] `union` doCriticalPoints x a (b-1) expr 
                              in
                              nub (map fixFloatingError (catMaybes $ doCriticalPoints x a b expr))

  
  extrema x a b expr = let 
                  doExtrema _ [] _ = []
                  doExtrema x ((Const a):as) expr = let 
                      derivative = partDiff x expr
                      y = ((fromInteger $ round $ (eval (Map.fromList [(x, a)]) expr) * (10^6)) / (10.0^^6))
                      in 
                        if eval (Map.fromList [(x, a-1e-5)]) derivative < 0 &&  eval (Map.fromList [(x, a+1e-5)]) derivative > 0
                        then doExtrema x as expr ++ [("Local Minimum", (a, y))]
                        else if eval (Map.fromList [(x, a-1e-5)]) derivative > 0 &&  eval (Map.fromList [(x, a+1e-5)]) derivative < 0
                        then doExtrema x as expr ++ [("Local Maximum", (a, y))]
                        else [] ++ doExtrema x as expr
                  in
                  doExtrema x (criticalPoints x a b expr) expr

  fixFloatingError (Const x) = (Const ((fromInteger $ round $ x * (10^6)) / (10.0^^6)))
  fixFloatingError _ = error "Only constants are supported."

  pureNewton x s maxx p = case (length $ getVars p) of
                      0 -> Nothing
                      1     | x /= (head $ getVars p) -> Nothing
                            | maxx == 0 -> Nothing
                            | (pureEval (Map.fromList [(x, s)]) $ partDiff x p) == Just 0 -> Nothing
                            | otherwise -> case (pureEval (Map.fromList [(x, s)]) p, pureEval (Map.fromList [(x, s)]) $ partDiff x p) of
                                  (Just a, Just b)    | abs a <= (0 + 1e-10) -> Just (Const s) 
                                                      | otherwise -> pureNewton x (s - a / b) (maxx - 1) p
                                  (Nothing, _) -> Nothing
                                  (_, Nothing) -> Nothing
                      _ -> Nothing