module Chapter11Huttons where

data Expr =
  Lit Integer |
  Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add expr1 expr2) = eval expr1 + eval expr2

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add expr1 expr2) = printExpr expr1 ++ " + " ++ printExpr expr2
