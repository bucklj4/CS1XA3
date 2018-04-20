module ExprParser (parseExprD,parseExprF) where

import           ExprType
import           ExprPretty
import           ExprDiff
import qualified Data.Map.Strict as Map

import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1, between)
import Text.Parsec (letter, char, digit, string, oneOf, option, parse)

import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many, (<$))
import Control.Monad 

import qualified Text.Parsec.Expr as E

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b


parseExprD :: String -> Expr Double
parseExprD ss = case parse exprD "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

parseExprF :: String -> Expr Float
parseExprF ss = case parse exprF "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

exprD :: Parser (Expr Double)
exprD =  E.buildExpressionParser pteTable pteTermD 

exprF :: Parser (Expr Float)
exprF = E.buildExpressionParser pteTable pteTermF 

-- This is the order of associativities from most associative to least.
pteTable = [
            [E.Infix (Power <$ symbol "^") E.AssocLeft],
            [E.Infix (Mult <$ symbol "*") E.AssocLeft, E.Infix (Div <$ symbol "/") E.AssocLeft]
           ,[E.Infix (Add <$ symbol "+") E.AssocLeft, E.Infix (Subtr <$ symbol "-") E.AssocLeft]
           ]

-- PARSER FOR DOUBLES --
-- Goes through each subparser
pteTermD ::  Parser (Expr Double)
pteTermD = pteSinD <|> pteCosD <|> pteTanD <|> pteLogD <|> pteExpD <|> pteVarD <|> pteNumD <|> pteParensD

-- Parse a number
pteNumD ::  Parser (Expr Double)
pteNumD = Const <$> realD

-- Just return the string of the variable
pteVarD ::  Parser (Expr Double)
pteVarD = Var <$> identifier

-- Gets what's between brackets
pteParensD ::  Parser (Expr Double)
pteParensD = Parens <$> between (symbol "(") (symbol ")") exprD

-- Gets what's inside the sine function
pteSinD ::  Parser (Expr Double)
pteSinD =  Sin <$> between (symbol "sin(") (symbol ")") exprD

-- Gets what's inside the cosine function
pteCosD ::  Parser (Expr Double)
pteCosD =  Cos <$> between (symbol "cos(") (symbol ")") exprD

-- Gets what's inside the tangent function
pteTanD ::  Parser (Expr Double)
pteTanD =  Tan <$> between (symbol "tan(") (symbol ")") exprD

-- Gets what's inside the natural log function
pteLogD ::  Parser (Expr Double)
pteLogD =  Log <$> between (symbol "ln(") (symbol ")") exprD

-- Gets what's inside the natural exponential function
pteExpD ::  Parser (Expr Double)
pteExpD =  Exp <$> between (symbol "e^(") (symbol ")") exprD

-- Used so that negative numbers can be parsed
realD ::  Parser Double
realD = lexeme double

double = fmap rd $ fullNumber <++> decimal
    where rd      = read :: String -> Double
          decimal = option "" $ char '.' <:> number


---------------------------------------------------------------------------------------------------------------------

-- PARSER FOR FLOATS -- 
-- Goes through each subparser
pteTermF ::  Parser (Expr Float)
pteTermF = pteSinF <|> pteCosF <|> pteTanF <|> pteLogF<|> pteExpF <|> pteVarF <|> pteNumF <|> pteParensF

-- Parse a number
pteNumF ::  Parser (Expr Float)
pteNumF = Const <$> realF

-- Just return the string of the variable
pteVarF ::  Parser (Expr Float)
pteVarF = Var <$> identifier

-- Gets what's between brackets
pteParensF ::  Parser (Expr Float)
pteParensF = Parens <$> between (symbol "(") (symbol ")") exprF

-- Gets what's inside the sine function
pteSinF ::  Parser (Expr Float)
pteSinF =  Sin <$> between (symbol "sin(") (symbol ")") exprF

-- Gets what's inside the cosine function
pteCosF ::  Parser (Expr Float)
pteCosF =  Cos <$> between (symbol "cos(") (symbol ")") exprF

-- Gets what's inside the tangent function
pteTanF ::  Parser (Expr Float)
pteTanF =  Cos <$> between (symbol "tan(") (symbol ")") exprF

-- Gets what's inside the natural log function
pteLogF ::  Parser (Expr Float)
pteLogF =  Log <$> between (symbol "ln(") (symbol ")") exprF

-- Gets what's inside the natural exponential function
pteExpF ::  Parser (Expr Float)
pteExpF =  Exp <$> between (symbol "e^(") (symbol ")") exprF

-- Used so that negative numbers can be parsed
realF ::  Parser Float
realF = lexeme float

float = fmap rd $ fullNumber <++> decimal
    where rd      = read :: String -> Float
          decimal = option "" $ char '.' <:> number

----------------------------------------------------------------------------------------------------------------------

-- Remove whitespace
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme ::  Parser a -> Parser a
lexeme p = p <* whitespace

-- Combines number with its sign
fullNumber = plus <|> minus <|> number

number = many1 digit

plus = char '+' *> number

minus = char '-' <:> number

-- Variables must start with a letter or _
identifier :: Parser String
identifier = lexeme ((:) <$> firstChar <*> many nonFirstChar)
  where
    firstChar = letter <|> char '_'
    nonFirstChar = digit <|> firstChar

-- Simple string parser
symbol :: String -> Parser String
symbol s = lexeme $ string s