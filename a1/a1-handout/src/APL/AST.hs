module APL.AST
  ( VName,
    Exp (..),
    printExp,
  )
where

type VName = String

data Exp
  = CstInt Integer
  | CstBool Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | Eql Exp Exp
  | If Exp Exp Exp
  | Var VName
  | Let VName Exp Exp
  | Lambda VName Exp
  | Apply Exp Exp
  | TryCatch Exp Exp
  deriving (Eq, Show)

printBinOp :: Exp -> Exp -> String -> String
printBinOp e1 e2 binOp = printExp e1 ++ " " ++ binOp ++ " " ++ printExp e2

printExp :: Exp -> String
printExp (CstInt n) = show n
printExp (CstBool b) = show b
printExp (Var v) = v
printExp (Add e1 e2) = "(" ++ printBinOp e1 e2 "+" ++ ")"
printExp (Sub e1 e2) = "(" ++ printBinOp e1 e2 "-" ++ ")"
printExp (Mul e1 e2) = "(" ++ printBinOp e1 e2 "*" ++ ")"
printExp (Div e1 e2) = "(" ++ printBinOp e1 e2 "/" ++ ")"
printExp (Pow e1 e2) = "(" ++ printBinOp e1 e2 "**" ++ ")"
printExp (Eql e1 e2) = "(" ++ printBinOp e1 e2 "==" ++ ")"
printExp (Let v e1 e2) = "(let " ++ v ++ " = " ++ printExp e1 ++ " in " ++ printExp e2 ++ ")"
printExp (If cond e1 e2) = "(if " ++ printExp cond ++ " then " ++ printExp e1 ++ " else " ++ printExp e2 ++ ")"
printExp (TryCatch e1 e2) = "(try " ++ printExp e1 ++ " catch " ++ printExp e2 ++ ")"
printExp (Lambda n e) = "(\\" ++ n ++ " -> " ++ printExp e ++ ")"
printExp (Apply funExp argExp) = "(" ++ printExp funExp ++ printExp argExp ++ ")"