module Main where

import RayTracing
import Graphics.Image as Img
import Graphics.Image.IO (writeImageExact)
import Linear.V3

main :: IO ()
main = do 
    let objs = [Sphere (V3 0.0 0.0 13.0) 3.0, Sphere (V3 0.0 (-103.5) 13.0) 100]
    img  <- renderWorld 0.0 (16/9) 16 800 10.0 objs
    writeImageExact PNG [] "world.png" img
