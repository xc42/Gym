module TinyThreePassCompiler where

import qualified Data.Map as Map

data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         deriving (Eq, Show)

data Token = TChar Char
           | TInt Int
           | TStr String
           deriving (Eq, Show)


alpha, digit :: String
alpha = ['a'..'z'] ++ ['A'..'Z']
digit = ['0'..'9']

tokenize :: String -> [Token]
tokenize [] = []
tokenize xxs@(c:cs)
  | c `elem` "-+*/()[]" = TChar c : tokenize cs
  | not (null i) = TInt (read i) : tokenize is
  | not (null s) = TStr s : tokenize ss
  | otherwise = tokenize cs
  where
    (i, is) = span (`elem` digit) xxs
    (s, ss) = span (`elem` alpha) xxs

compile :: String -> [String]
compile = pass3 . pass2 . pass1

pass1 :: String -> AST
pass1 src = 
    let tks = tokenize src
        (paramList, expr) = span (TChar ']' /=) (tail tks)
        table = Map.fromList $ zip [s | (TStr s) <- filter isVar paramList] [0..]
        shuntingYard :: [Token] -> [AST] -> [Token] -> AST
        shuntingYard (TInt n:ts) astStk opStk = shuntingYard ts (Imm n:astStk) opStk
        shuntingYard (TStr s:ts) astStk opStk = shuntingYard ts (Arg (Map.findWithDefault (-1) s table):astStk) opStk
        shuntingYard (TChar '(':ts) astStk opStk = shuntingYard ts astStk (TChar '(':opStk)
        shuntingYard (TChar ')':ts) astStk opStk = let (lop, rop) = span (TChar '(' /= ) opStk in 
                                                       shuntingYard ts (buildAST astStk lop) (tail rop)
        shuntingYard (TChar op:ts) astStk opStk = let (lop, rop) = span (\(TChar p) -> prec p >= prec op) opStk in
                                                      shuntingYard ts (buildAST astStk lop) (TChar op:rop)

        shuntingYard [] astStk opStk = head $ buildAST astStk opStk
    in shuntingYard (tail expr) [] []

   where buildAST astStk [] = astStk
         buildAST (x:y:stk) ((TChar op):ops) = case op of 
                                                 '+' -> buildAST (Add y x:stk) ops
                                                 '-' -> buildAST (Sub y x:stk) ops
                                                 '*' -> buildAST (Mul y x:stk) ops
                                                 '/' -> buildAST (Div y x:stk) ops

         isVar (TStr _) = True
         isVar _ = False
         prec '+' = 1
         prec '-' = 1
         prec '*' = 2
         prec '/' = 2 
         prec '(' = 0
                      
pass2 :: AST -> AST   
pass2 (Add e1 e2) = let (e1', e2') = (pass2 e1, pass2 e2) in
                        case (e1', e2') of
                          (Imm n1, Imm n2) -> Imm (n1+n2)
                          (_, _) -> Add e1' e2'

pass2 (Sub e1 e2) = let (e1', e2') = (pass2 e1, pass2 e2) in
                        case (e1', e2') of
                          (Imm n1, Imm n2) -> Imm (n1-n2)
                          (_, _) -> Sub e1' e2'

pass2 (Mul e1 e2) = let (e1', e2') = (pass2 e1, pass2 e2) in
                        case (e1', e2') of
                          (Imm n1, Imm n2) -> Imm (n1*n2)
                          (_, _) -> Mul e1' e2'

pass2 (Div e1 e2) = let (e1', e2') = (pass2 e1, pass2 e2) in
                        case (e1', e2') of
                          (Imm n1, Imm n2) -> Imm (div n1 n2)
                          (_, _) -> Div e1' e2'
pass2 e = e


pass3 :: AST -> [String]
pass3 expr = codegen expr []
    where 
        codegen :: AST -> [String] -> [String]
        codegen (Imm n) next = ("IM " ++ show n):next
        codegen (Arg n) next = ("AR " ++ show n):next
        codegen (Add e1 e2) next = codegen e1 ("PU":codegen e2 ("SW":"PO":"AD":next))
        codegen (Sub e1 e2) next = codegen e1 ("PU":codegen e2 ("SW":"PO":"SU":next))
        codegen (Mul e1 e2) next = codegen e1 ("PU":codegen e2 ("SW":"PO":"MU":next))
        codegen (Div e1 e2) next = codegen e1 ("PU":codegen e2 ("SW":"PO":"DI":next))
