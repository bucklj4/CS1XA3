module ExprType where

import           Data.List

data Expr a =     Add (Expr a) (Expr a)
                | Subtr (Expr a) (Expr a)
                | Mult (Expr a) (Expr a)
                | Div (Expr a) (Expr a)
                | Cos (Expr a)
                | Sin (Expr a)
                | Tan (Expr a)
                | Log (Expr a)
                | Exp (Expr a)
                | Const a
                | Var String
                | Parens (Expr a)
                | Power (Expr a) (Expr a)
  deriving Eq

-- Returns a list of variables using sets
getVars :: Expr a -> [String]
getVars (Add e1 e2)  = getVars e1 `union` getVars e2
getVars (Subtr e1 e2)  = getVars e1 `union` getVars e2
getVars (Mult e1 e2) = getVars e1 `union` getVars e2
getVars (Div e1 e2) = getVars e1 `union` getVars e2
getVars (Power e1 e2) = getVars e1 `union` getVars e2
getVars (Cos e1) = getVars e1
getVars (Sin e1) = getVars e1
getVars (Tan e1) = getVars e1
getVars (Log e1) = getVars e1
getVars (Exp e1) = getVars e1
getVars (Parens e1) = getVars e1
getVars (Const _)    = []
getVars (Var ident)  = [ident]
