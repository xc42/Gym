module Main where

import Test.HUnit

import LangCore as LC
import TypeInfer
import LangParser
import Text.Printf

assertAst :: String -> Expr -> IO ()
assertAst str ast = assertEqual msg (Right ast) (parseSexp str)
    where msg = printf "\"%s\" failed to parse as \"%s\"" str (show ast)

assertExprType :: Expr -> LC.Type -> IO ()
assertExprType expr ty = assertEqual msg  ty (infer expr)
    where msg = printf "%s should has type: %s" (show expr) (show ty)

assertExprType2 :: String -> LC.Type -> IO()
assertExprType2 s ty = case parseSexp s of
                        Left _ -> assertFailure "expr parse fail"
                        Right expr -> assertExprType expr ty

parserTest :: Test
parserTest = TestList
    [TestCase $ assertAst "4" (Number 4),
     TestCase $ assertAst "#t" (Boolean True),
     TestCase $ assertAst "xyz" (Var "xyz"),
     TestCase $ assertAst "(if #t #f 42)" (If (Boolean True) (Boolean False) (Number 42)),
     TestCase $ assertAst "(+ 42 x)" (BPrim Add (Number 42) (Var "x")),
     TestCase $ assertAst "(not #t)" (UPrim Not (Boolean True)),
     TestCase $ assertAst "(let ((x y)) x)" (Let [("x", Var "y")] (Var "x")),
     TestCase $ assertAst "(f g)" (App (Var "f") [Var "g"]),
     TestCase $ assertAst "(lambda (x) (x x))" (Lambda ["x"] (App (Var "x") [Var "x"])),
     TestCase $ assertAst "(let ((x 1) (y (f x))) (> x y))"
                          (Let [("x", Number 1), ("y", App (Var "f") [Var "x"])]
                               (BPrim LC.GT (Var "x") (Var "y"))) ]

inferTest :: Test
inferTest = TestList 
    [TestCase $ assertExprType (Number 4) TyInt,
     TestCase $ assertExprType (Boolean True) TyBool,
     TestCase $ assertExprType (If (Boolean True) (Number 2) (Number 1)) TyInt,
     TestCase $ assertExprType2 "(let ((x 2)) (+ x x))" TyInt,
     TestCase $ assertExprType2 "(lambda (x) (- 5 x))" (TyFunc [TyInt] TyInt),
     TestCase $ assertExprType2 "(lambda (x y) (> x y))" (TyFunc [TyInt, TyInt] TyBool),
     TestCase $ assertExprType2 "((lambda (x) x) 0)" TyInt,
     TestCase $ assertExprType2 "(lambda (x) (* x x))" (TyFunc [TyInt] TyInt),
     TestCase $ assertExprType2 "(lambda (x y) (+ x y))" (TyFunc [TyInt, TyInt] TyInt),
     TestCase $ assertExprType2 "(let ((id (lambda (x) x))) (let ((y (id #t))) id))" (TyFunc [TyBool] TyBool),
     TestCase $ assertExprType2 "(let ((c (lambda (f g)\
                                            \(lambda (x) (f (g x)))))\
                                        \(id (lambda (x) x)))\
                                            \(let ((v (id 0)))\
                                                \(c id id)))" (TyFunc [TyInt] TyInt)
     ]

main = do
    runTestTT parserTest
    runTestTT inferTest

