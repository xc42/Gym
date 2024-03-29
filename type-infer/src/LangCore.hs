module LangCore where


data Expr = 
    Number Int
      | Boolean Bool
      | Var String
      | If Expr Expr Expr
      | Let [(String, Expr)] Expr
      | Lambda [String] Expr
      | App Expr [Expr]
      | BPrim BinOp Expr Expr
      | UPrim UOp Expr
      deriving (Show, Eq)

data BinOp = Add | Sub | Mul | Div | Mod
        | Equal | LT| GT | LE | GE
        | And | Or 
      deriving (Show, Ord, Eq)


data UOp = Not
      deriving (Show, Eq)


data Type =
    TyInt
      | TyBool
      | TyFunc [Type] Type
      | TVar Int
      | QVar Int
      deriving (Show, Eq)
