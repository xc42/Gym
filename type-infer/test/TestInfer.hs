module Main where

import Test.HUnit

import LangCore as LC
import TypeInfer
import Text.Printf

assertExprType :: Expr -> LC.Type -> IO ()
assertExprType expr ty = assertEqual msg (infer expr) ty
    where msg = printf "%s should has type: %s" (show expr) (show ty)

basicTests :: Test
basicTests = TestList 
    [TestCase $ assertExprType (Number 4) TyInt,
     TestCase $ assertExprType (Boolean True) TyBool ]

main = runTestTT basicTests
