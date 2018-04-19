module ExprPretty where

import           ExprType

parens :: String -> String
parens ss = "(" ++ ss ++ ")"

-- This is how things are displayed. It makes things easier to read
instance Show a => Show (Expr a) where
  show (Mult e1 e2) = parens (show e1) ++ " !* " ++ parens (show e2)
  show (Add e1 e2)  = parens (show e1) ++ " !+ " ++ parens (show e2)
  show (Subtr e1 e2)= parens (show e1) ++ " !- " ++ parens (show e2)
  show (Div e1 e2)  = parens (show e1) ++ " !/ " ++ parens (show e2)
  show (Power e1 e2)= parens (show e1) ++ " !^ " ++ parens (show e2)
  show (Sin e1)     = parens $ "sine " ++ show e1
  show (Cos e1)     = parens $ "cosine " ++ show e1
  show (Tan e1)     = parens $ "tangent " ++ show e1
  show (Log e1)     = parens $ "ln " ++ show e1
  show (Exp e1)     = parens $ "e " ++ show e1
  show (Parens e1)  = parens (show e1)
  show (Var ss)     = parens $ "var \"" ++ ss ++ "\""
  show (Const x)    = parens $ "val " ++ show x
