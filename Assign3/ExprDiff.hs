{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}

module ExprDiff where

import           ExprType
import           Data.List
import           Data.Fixed
import           Data.Maybe

import qualified Data.Map.Strict as Map

isInt x = x == fromInteger (round x)

class DiffExpr a where
  eval :: Map.Map String a -> Expr a -> a
  simplify :: Map.Map String a -> Expr a -> Expr a
  reduce :: Expr a -> Expr a
  partDiff :: String -> Expr a -> Expr a
  
  (!+) :: Expr a -> Expr a -> Expr a
  e1 !+ e2 = reduce $ Add e1 e2

  (!-) :: Expr a -> Expr a -> Expr a
  e1 !- e2 = reduce $ Subtr e1 e2

  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = reduce $ Mult e1 e2

  (!/) :: Expr a -> Expr a -> Expr a
  e1 !/ e2 = reduce $ Div e1 e2

  (!^) :: Expr a -> Expr a -> Expr a
  e1 !^ e2 = reduce $ Power e1 e2

  cosine :: Expr a -> Expr a
  cosine e1 = reduce $ Cos e1

  sine :: Expr a -> Expr a
  sine e1 = reduce $ Sin e1

  tangent :: Expr a -> Expr a
  tangent e1 = reduce $ Tan e1

  ln :: Expr a -> Expr a
  ln e1 = reduce $ Log e1

  e :: Expr a -> Expr a
  e e1 = reduce $ Exp e1

  val :: a -> Expr a
  val x = Const x

  var :: String -> Expr a
  var x = Var x

instance (Num a, Eq a, Fractional a, Floating a, Ord a, Show a, RealFrac a) => DiffExpr a where
  -- This comes in handy when we just want to try to simplify an expression without passing it any variables
  reduce x = simplify (Map.fromList []) x 



  -- The base case for eval
  eval vrs (Const x) = x

  -- Recursive definitions used for Add, Mult, and Div here
  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
  eval vrs (Div e1 e2) = if eval vrs e2 /= 0 then eval vrs e1 / eval vrs e2 else error "The denominator equals zero!"

  -- Using Haskell's built-in math functions to evaluate
  eval vrs (Sin x) = sin(eval vrs x)
  eval vrs (Cos x) = cos(eval vrs x)
  eval vrs (Exp x) = exp(eval vrs x)

  -- The natural log of the natural exponential function raised to anything is simply anything.
  -- Note: this is not the same in reverse, as the log of negative number is undefined, so the entire function
  -- will not be defined on all of its domain
  eval vrs (Log (Exp x)) = eval vrs x
  eval vrs (Log x) = if eval vrs x > 0 then log(eval vrs x) else error "Log is undefined on this domain!"

  -- We cannot evaluate the expression if we cannot substitute all variables
  eval vrs (Var x) = case Map.lookup x vrs of
                       Just v  -> v
                       Nothing -> error "The given variables are not in the expression!"

  -- For any other complex expression, we should reduce it first so that we can operate on it.
  -- For instance, in the library powers will simply be reduced to repeated multiplication                     
  eval vrs (y) = eval vrs (reduce y)




  -- The base case for simplify
  simplify vrs (Const x) = (Const x)

  -- We do not all variables to be substituted in simplify, but we can substitute what we are given
  simplify vrs (Var x) = case Map.lookup x vrs of
                       Just v  -> (Const v)
                       Nothing -> (Var x)

  -- Defining how we shoud simplify addition expressions
  simplify vrs (Add a b) =
    case (simplify vrs a, simplify vrs b) of
      -- Adding zero to anything does nothing, so just continue simplifying the rest of the expression
      (Const 0, x) -> simplify vrs x
      (x, Const 0) -> simplify vrs x

      -- Standard addition of two numbers
      (Const x, Const y) -> Const (x+y)

      -- We can't really do much to variables added to constants
      (Const x, Var y) -> (Add (simplify vrs (Var y)) (Const x))
      (Var y, Const x) -> (Add (simplify vrs (Var y)) (Const x))

      -- Collecting like terms
      (Var a, Mult (Const b) (Var c)) -> if a == c then (Mult (Const (b+1)) (simplify vrs (Var a))) else (Add (simplify vrs (Var a)) (Mult (Const b) (simplify vrs (Var c))))
      (Mult (Const b) (Var c), Var a) -> if a == c then (Mult (Const (b+1)) (simplify vrs (Var a))) else (Add (simplify vrs (Var a)) (Mult (Const b) (simplify vrs (Var c))))

      -- To simplify as best as we can, we move any constants to the right of the expression so they can be grouped together
      (Const x, Add (Const y) z) -> simplify vrs (Add (simplify vrs z) (Const (x+y)))
      (Const x, Add z (Const y)) -> simplify vrs (Add (simplify vrs z) (Const (x+y)))
      (Add (Const y) z, Const x) -> simplify vrs (Add (simplify vrs z) (Const (x+y)))
      (Add z (Const y), Const x) -> simplify vrs (Add (simplify vrs z) (Const (x+y)))
      (Add (Const a) (Var b), d) -> (Add (simplify vrs (Add (simplify vrs (Var b)) (simplify vrs d))) (Const a))
      (Add (Var b) (Const a), d) -> (Add (simplify vrs (Add (simplify vrs (Var b)) (simplify vrs d))) (Const a))
      (d, Add (Const a) (Var b)) -> (Add (simplify vrs (Add (simplify vrs (Var b)) (simplify vrs d))) (Const a))
      (d, Add (Var b) (Const a)) -> (Add (simplify vrs (Add (simplify vrs (Var b)) (simplify vrs d))) (Const a))

      -- Collecting like terms on variables
      (Var a, Add (Var b) (Var c)) -> if a == b then (Add (Mult (Const 2) (simplify vrs (Var a))) (simplify vrs (Var c))) else if a == c then (Add (Mult (Const 2) (simplify vrs (Var a))) (simplify vrs (Var b))) else (Add (simplify vrs (Var a)) (Add (simplify vrs (Var b)) (simplify vrs (Var c))))
      (Var a, Add (Var b) c) -> if a == b then (Add (Mult (Const 2) (simplify vrs (Var a))) (simplify vrs c)) else (Add (simplify vrs (Var a)) (Add (simplify vrs (Var b)) (simplify vrs c)))
      (Var a, Add c (Var b)) -> if a == b then (Add (Mult (Const 2) (simplify vrs (Var a))) (simplify vrs c)) else (Add (simplify vrs (Var a)) (Add (simplify vrs (Var b)) (simplify vrs c)))
      (Add (Var b) c, Var a) -> if a == b then (Add (Mult (Const 2) (simplify vrs (Var a))) (simplify vrs c)) else (Add (simplify vrs (Var a)) (Add (simplify vrs (Var b)) (simplify vrs c)))
      (Add c (Var b), Var a) -> if a == b then (Add (Mult (Const 2) (simplify vrs (Var a))) (simplify vrs c)) else (Add (simplify vrs (Var a)) (Add (simplify vrs (Var b)) (simplify vrs c)))  
      (Var a, Var b) -> if a == b then (Mult (Const 2) ((simplify vrs (Var a)))) else (Add (simplify vrs (Var a)) (simplify vrs (Var b)))

      -- Worst case scenario in which we can't do anything: leave it as is
      (x, y) -> (Add x y)




  -- Defining how we shoud simplify multiplication expressions
  simplify vrs (Mult a b) =
    case (simplify vrs a, simplify vrs b) of
      -- Anything times zero is zero
      (Const 0, y) -> (Const 0)
      (y, Const 0) -> (Const 0)

      --Anything times one is itself
      (Const 1, y) -> (simplify vrs y)
      (y, Const 1) -> (simplify vrs y)

      -- Standard multiplication of two numbers
      (Const x, Const y) -> Const (x*y)

      -- To simplify as best as we can, we move any constants to the right of the expression so they can be grouped together
      (Const x, Mult (Const y) z) -> simplify vrs (Mult (simplify vrs z) (Const (x*y)))
      (Const x, Mult z (Const y)) -> simplify vrs (Mult (simplify vrs z) (Const (x*y)))
      (Mult (Const y) z, Const x) -> simplify vrs (Mult (simplify vrs z) (Const (x*y)))
      (Mult z (Const y), Const x) -> simplify vrs (Mult (simplify vrs z) (Const (x*y)))
      (Mult (Const a) (Var b), d) -> (Mult (simplify vrs (Mult (Var b) (simplify vrs d))) (Const a))
      (Mult (Var b) (Const a), d) -> (Mult (simplify vrs (Mult (Var b) (simplify vrs d))) (Const a))
      (d, Mult (Const a) (Var b)) -> (Mult (simplify vrs (Mult (Var b) (simplify vrs d))) (Const a))
      (d, Mult (Var b) (Const a)) -> (Mult (simplify vrs (Mult (Var b) (simplify vrs d))) (Const a))

      -- Worst case scenario in which we can't do anything: leave it as is
      (x, y) -> (Mult x y)


  -- We cannot do much simplification to division statements because of their complexity
  simplify vrs (Div a b) =
    case (simplify vrs a, simplify vrs b) of
      -- Zero divided by anything is 0
      (Const 0, y) -> (Const 0)

      -- We cannot divide by zero
      (y, Const 0) -> error "Division by zero error!" 

      -- Standard division of two numbers
      (Const x, Const y) -> Const (x/y)

      -- Simple variable cancellation
      (Var x, Var y) -> if x == y then (Const 1) else (Div (Var x) (Var y))
      (x, y) -> (Div x y)

  -- If we have a number, we can find the cosine of it; if we have a variable, we leave it as is; otherwise, we can only simplify inside the cosine function     
  simplify vrs (Cos (Const a)) = (Const (cos(a)))
  simplify vrs (Cos (Var a)) = (Cos (Var a))
  simplify vrs (Cos a) = (Cos (simplify vrs a))

  -- If we have a number, we can find the sine of it; if we have a variable, we leave it as is; otherwise, we can only simplify inside the sine function    
  simplify vrs (Sin (Const a)) = (Const (sin(a)))
  simplify vrs (Sin (Var a)) = (Sin (Var a))
  simplify vrs (Sin a) = (Sin (simplify vrs a))

  -- If we have a number, we can find the natural log of it; if we have a variable, we leave it as is; if we have the natural exponential, we cancel it; otherwise, we can only simplify inside the natural log function  
  simplify vrs (Log (Const a)) = if a > 0 then (Const (log(a))) else error "Log is undefined on this domain!"
  simplify vrs (Log (Var a)) = (Log (Var a))
  simplify vrs (Log (Exp a)) = simplify vrs (Exp a)
  simplify vrs (Log a) = (Log (simplify vrs a))

  -- If we have a number, we can find the sine of it; if we have a variable, we leave it as is; otherwise, we can only simplify inside the sine function  
  simplify vrs (Exp (Const a)) = (Const (exp(a)))
  simplify vrs (Exp (Var a)) = (Exp (Var a))
  simplify vrs (Exp a) = (Exp (simplify vrs a))

  -- We're just going to convert a subtaction expression into an addition expression
  simplify vrs (Subtr a b) = simplify vrs (Add (simplify vrs a) (simplify vrs (Mult (Const (-1)) b)))

  -- Parentheses don't do much
  simplify vrs (Parens a) = simplify vrs a

  -- Powers reduce to repeated multiplication
  simplify vrs (Power a (Const 0)) = (Const 1)
  simplify vrs (Power a (Const 1)) = reduce $ simplify vrs a
  simplify vrs (Power a (Const k)) = if k > 0 && isInt k then reduce $ (Mult (simplify vrs a) (simplify vrs (Power (simplify vrs a) (Const (k-1))))) else if k < 0 && isInt k then reduce $ simplify vrs (Div (Const 1) (Power (simplify vrs a) (Const (abs(k))))) else reduce $ simplify vrs (Exp (Mult (Const k) (Log (simplify vrs a))))
  simplify vrs (Power a b) = reduce $ simplify vrs (Exp (Mult (simplify vrs b) (Log (simplify vrs a))))

  -- Tangents can be reduced to ratios of sine to cosine
  simplify vrs (Tan a) = reduce $ simplify vrs (Div (Sin (simplify vrs a)) (Cos (simplify vrs a)))




  -- Constants become zero in derivatives
  partDiff x (Const a) = (Const 0)
  
  -- If we're differentiating with respect to one variable, all other variables are treated as constants
  partDiff x (Var a) = if x == a then (Const 1) else (Const 0)

  -- Addition acts as normal
  partDiff x (Add a b) = (partDiff x a) !+ (partDiff x b)

  -- Power rule
  partDiff x (Mult (Const a) b) = (Const a) !* (partDiff x b)
  partDiff x (Mult (Var a) (Var b)) = if a == b && a == x then (Const 2) !* (Var x) else if a == b && a /= x then (Const 0) else if a /= b && x == a then (Var b) else (Var a)


  -- Product rule 
  partDiff x (Mult a b) = simplify (Map.fromList []) $ ((partDiff x a) !* (reduce b)) !+ ((reduce a) !* (partDiff x b))

  -- Quotient rule
  partDiff x (Div a b) = reduce $ (((reduce b) !* (partDiff x a)) !- ((reduce a) !* partDiff x b)) !/ ((reduce b) !* (reduce b))

  -- Special function differentiation 
  partDiff x (Cos (Var a)) = if x == a then (Const (-1)) !* (Sin (Var a)) else  (Const 0)
  partDiff x (Sin (Var a)) = if x == a then reduce (Cos (Var a)) else (Const 0)
  partDiff x (Log (Var a)) = if x == a then reduce ((Const 1) !/ (Var a)) else (Const 0)
  partDiff x (Exp (Var a)) = if x == a then reduce (Exp (Var a)) else (Const 0)

  -- Special function differentiation with chain rule
  partDiff x (Cos a) = (Const (-1)) !* (Sin (reduce a)) !* (partDiff x a)
  partDiff x (Sin a) = ((Cos (reduce a)) !* (partDiff x a))
  partDiff x (Log a) = (((Const 1) !/ (reduce a)) !* (partDiff x a))
  partDiff x (Exp a) = ((Exp (reduce a)) !* (partDiff x a))

  -- If we can't differentiate, first reduce
  partDiff x y = partDiff x (reduce y)

  


  