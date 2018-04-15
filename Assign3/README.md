# Custom Math Library

This is a custom math library written in Haskell that can be used to encode mathematical expressions so that they can be simplified, evaluated, or even partially differentiated. The library also includes some handy tools for all your calculus midterm studying needs! 

## Getting Started
You'll find the different aspects of the math library split up into neatly contained modules. These include: ```ExprType.hs``` for the skeleton of mathematical expressions, ```ExprDiff.hs``` for the nitty-gritty of the mathematical functions, ```ExprParser.hs``` for parsing strings into mathematical expressions that can be operated on by the library, and ```ExprTest.hs``` for testing of the mathematical library.

### Prerequisites

What things you need to install the software and how to install them

```
TBA
```

### Installing

TBA

## Mathematical Expressions

The following mathematical functions are encoded in the library:
```haskell
(Const 64.0)                          -- Encodes a constant as a Float or Double

(Var "x")                             -- Encodes a variable as a String

(Parens (Expression))                 -- Wraps an expression in parantheses

(Add (Expression) (Expression))       -- Adds two expressions together

(Expression) !+ (Expression)          -- Infix notation for addition

(Subtr (Expression) (Expression))     -- The difference of two expressions

((Expression) !- (Expression))        -- Infix notation for subtraction

(Mult (Expression) (Expression))      -- Multiplies two expressions together

((Expression) !* (Expression))        -- Infix notation for multiplication

(Div (Expression) (Expression))       -- Divides two expressions (denominator cannot equal zero)

((Expression !/ (Expression))         -- Infix notation for division

((Sin (Expression))                   -- Applies an expression to the sine function

((Cos (Expression))                   -- Applies an expression to the cosine function

((Exp (Expression))                   -- Applies an expression to the natural exponential function

((Log (Expression))                   -- Applies an expression to the natural logarithm function
```
### Core Functionality
The two core functions of the library are `simplify` and `eval`. The functions take the same two parameters:
  * A Map of variables to their corresponding values (if you wish to substitute) in a tuple. The Math.fromList function can be used to   convert a list of these tuples to a Map:
  ```haskell
  (Map.fromList [("x", 19), ("y", 98)]) -- Variable "x" will be given a value of 19 and "y" a value of 98
  
  (Map.fromList []) -- If we do not wish to substitute any variables (only allowed on simplify)
  ```
  * An encoded mathematical expression:
  ```haskell
  -- An expression representing the polynomial 9x^2 + 8x + 2
  ((Add (Mult (Const 9) (Mult (Var "x") (Var "x"))) (Add (Mult (Const 8) (Var "x")) (Const 2))))
  
  -- The same expression using infix notation
  (((val 9)) !* (((var "x")) !* ((var "x")))) !+ ((((val 8)) !* ((var "x"))) !+ ((val 2)))
  ```
  
  #### Simplify
  Simplify tries to reduce a mathematical expression as much as possible. You can pass it values to substitute for its variables should you wish to do so. This means simplify reduces an expression and keeps it as an encoded mathematical expression:
```haskell
simplify (Map.fromList []) ((Var "x")  !+ (Var "y") !+ (Const 2) !+ (Const 3))
>>> (((var "x")) !+ ((var "y"))) !+ ((val 5.0))

simplify (Map.fromList [("y", 5)]) ((Var "x")  !+ (Var "y") !+ (Const 2) !+ (Const 3))
>>> ((var "x")) !+ ((val 10.0))
```

 #### Eval
  Eval computes a mathematical expression and returns the value as a native Double or Float. You must pass it values to substitute for all its encoded variables should the expression have any. This means eval solves and decodes a mathematical expression:
```haskell
eval (Map.fromList [("x", 5), ("y", 10.5)]) ((Var "x")  !+ (Var "y") !+ (Const 2) !+ (Const 3))
>>> 20.5

eval (Map.fromList [("x", 5)]) ((Var "x")  !+ (Var "y") !+ (Const 2) !+ (Const 3))
>>> *** Exception: failed lookup in eval
```

### Parsing
  The file `ExprParser.hs` has functions to allow you to parse a string into an encoded mathematical expression.
  The following are supported strings that can be parsed and how they will be translated into the encoded mathematical expression:
  ```haskell
  "2" >> (Const 2) -- Numbers will be converted to Floats or Doubles depending on the parser used
  "x" >> (Var "x") -- Variables must start with letters or _
  "2 + x" >> (Add (Const 2) (Var "x")) -- Whitespace is ignored
  "(2+x)" >> (Parens (Add (Const 2) (Var "x")))
  "5.5 - 10.3" >> (Subtr (Const 5.5) (Const 10.3))
  "y * -5.5" >> (Mult (Var "y") (Const (-5.5)))
  "x / y" >> (Div (Var "x") (Var "y"))
  "sin(x)" >> (Sin (Var "x")) -- input MUST be wrapped in parantheses, no whitespace between paranthesis and function
  "cos(5 + -3)" >> (Cos (Add (Const 5) (Const (-3)))) 
  "e^(x)" >> (Exp (Var "x"))
  "ln(ln(x))" >> (Log (Log (Var "x")))
  "2 + 5 * -3" >> (Add (Const 2) (Parens (Mult (Const 5) (Const (-3))))) -- BEDMAS is observed
  ```
#### parseExprD
Parses a string into the encoded mathematical form, converting the numbers to Doubles:
```haskell
parseExprD "2 + -0.999999999999999"
>>> ((val 2.0)) !+ ((val -0.999999999999999))
```
#### parseExprF
Parses a string into the encoded mathematical form, converting the numbers to Floats:
```haskell
parseExprF "2 + -0.999999999999999"
>>> ((val 2.0)) !+ ((val -1.0))
```

## Partial Diffentiation
The function `partDiff` takes two arguments: 
(1) A string of the variable to which respect is given
(2) The expression to be differentiated.
```haskell
partDiff "x" (Div (Const 1) (Var "x"))
>>> ((val -1.0)) !/ (((var "x")) !* ((var "x")))

partDiff "x" (Mult (Var "x") (Var "y"))
>>> (var "y")
```

## Combining Functions
To harness the real power of this library, one should remember that the values of functions can be piped into other functions. The notation `$` is really useful here:
```haskell
-- Find the slope of the function f(x) = 1/x at f(2)
eval (Map.fromList [("x", 2)]) $ partDiff "x" $ parseExprD "1/x"
>>> -0.25
```
## Bonus Features
### Graphing
The file `Graphing.hs` includes the function `graph` which takes 4 arguments:
(1) The variable to which respect is given 
(2) The left value of the domain 
(3) The right value of the domain 
(4) The encoded mathematical expression to be plotted

The function produces an HTML file called graph.HTML in the same directory as `Graphing.hs` that can be opened to reveal the function plotted on its domain in SVG graphics!

```haskell 
graph "x" (-5) (5) $ parseExprD "sin(x)"
```
This code yields the following graph in an HTML page:
<img src="./graph.svg"/>

```haskell
graph "x" (-20) (20) $ parseExprD "ln(e^(x))"
```
Likewise, this code yields:
<img src="./graph2.svg"/>

## Built With

* [Parsec Expr](https://hackage.haskell.org/package/parsec-3.1.13.0/docs/Text-Parsec-Expr.html) - The parsing framework

## Versioning

TBA

## Authors

* **Jack Buckley** - [bucklj4](https://github.com/bucklj4)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* [Jake Wheat's Intro to Parsing](https://jakewheat.github.io/intro_to_parsing/)
* [Curtis D'Alves's skeleton files](http://www.cas.mcmaster.ca/~dalvescb/#outline-container-orga8f4fcc)
