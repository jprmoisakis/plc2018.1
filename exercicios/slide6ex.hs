-- showExpr
data Expr = Lit Int
    | Add Expr Expr
    | Sub Expr Expr
    --deriving (Show)
exp1 = Lit 2
exp2 = Add (Lit 1) (Lit 2)
exp3 = Add (Lit 5) (Sub (Lit 7) (Lit 4))

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++ ")"
showExpr (Sub e1 e2) = "(" ++ showExpr e1 ++ " - " ++ showExpr e2 ++ ")"


