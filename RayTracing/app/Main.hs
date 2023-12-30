module Main where

import RayTracing
import Graphics.Image as Img
import Graphics.Image.IO (writeImageExact)
import Linear.V3

main :: IO ()
main = writeImageExact PNG [] "world.png" (renderWorld 0.0 (16/9) 16 800 1.0 (Sphere (V3 0.0 0.0 4.0) 3.0))
