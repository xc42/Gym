module Main where

import TypeInfer (infer)

main :: IO ()
main = show $ infer Number 4
