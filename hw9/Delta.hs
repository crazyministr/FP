module Delta where

data Expression = Constant Integer |
                  Variable String |
                  Function String |
                  If Expression Expression Expression |
                  Application Expression Expression |
                  Lambda String Expression |
                  Let [(String, Expression)] Expression deriving (Show, Eq)

delta :: Expression -> Expression
delta (Application (Application (Function "+") (Constant x)) (Constant y)) = Constant (x + y)
delta (Application (Application (Function "-") (Constant x)) (Constant y)) = Constant (x - y)
delta (Application (Application (Function "*") (Constant x)) (Constant y)) = Constant (x * y)
delta (Application (Application (Function func) exp1) exp2) = delta (Application (delta (Application (Function func) (delta exp1))) (delta exp2))
delta (Application exp1 exp2) = Application (delta exp1) (delta exp2)
delta (Lambda charList exp) = (Lambda charList (delta exp))
delta (If exp1 exp2 exp3) = If (delta exp1) (delta exp2) (delta exp3)
delta (Let charList exp) = Let (map (\(x, y)-> (x, delta y)) charList) (delta exp)
delta exp = exp
