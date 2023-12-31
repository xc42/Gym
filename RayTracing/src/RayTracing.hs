module RayTracing  where

import Control.Monad
import Linear.Affine
import Linear.V3
import Linear.Vector ( (^*), (*^))
import Linear.Metric 

import Graphics.Image as Img hiding (normalize)
import Graphics.Image.Interface (makeImageM)

import System.Random.Stateful
import Debug.Trace

type V3D = V3 Double
type Point3f = V3 Double

data Ray = Ray {oringin :: Point3f,direction :: V3D}

data Hit = Hit {hitPoint :: Point3f, normal :: V3D, frontHit :: Bool}
    deriving (Show)


class Object o where
    hitTest :: Ray -> o -> Maybe Hit


data Sphere = Sphere { center :: Point3f, radius :: Double }

instance Object Sphere where
    hitTest (Ray rA rb) (Sphere sC r) =
        let a = rb `dot` rb
            b = rb `dot` (rA - sC)
            c = (rA - sC) `dot` (rA - sC) - r * r
            disc =  b * b - a * c
        in if disc < 0 
              then Nothing 
              else let t = (-b - sqrt disc) / a
                       hp = rA + t *^ rb
                    in Just $ Hit hp (signorm (hp - sC)) False



renderWorld :: (Object o) => Point3f -> Double -> Int -> Int -> Double -> [o] -> IO (Image VS RGB Double)
renderWorld camera aspectRatio vpW imgW focal objs = 
    let vpH = fromIntegral vpW / aspectRatio
        imgH = fromIntegral imgW / aspectRatio
        scaleRatio = vpH / imgH
        u = V3 scaleRatio 0.0 0.0
        v = V3 0.0 (-scaleRatio) 0.0
        pixelO = camera + V3 (-0.5 * fromIntegral vpW) (0.5 * vpH) focal

        sampleRay row col n = replicateM n $ do 
            uMu <- uniform01
            vMu <- uniform01
            let p = pixelO + (vMu + fromIntegral row) *^ v + (uMu + fromIntegral col) *^ u
            return $ Ray camera (p - camera)

        renderPixel :: (Int, Int) -> IO (Pixel RGB Double)
        renderPixel (row, col) = do
            let sampleN = 10
            rays <- sampleRay row col sampleN
            let ratio = fromIntegral row / realToFrac imgH
            let bgColor = V3 (0.6*ratio) (0.8*ratio) (1.0*ratio)
            let (V3 r g b) = 1.0 / fromIntegral sampleN * foldl (\acc r -> acc + renderObj r objs bgColor) (V3 0.0 0.0 0.0) rays
            return $ PixelRGB r g b
    in  makeImageM (round imgH, imgW) renderPixel
    where 
        renderObj :: (Object o) => Ray -> [o] -> V3D -> V3D
        renderObj _ [] bgColor = bgColor
        renderObj ray@(Ray origin dir) (obj:rest) bgColor = case hitTest ray obj of
                                                      Just (Hit p nm front) -> let intensity = negate $ realToFrac $ signorm dir `dot` nm in
                                                                               V3 intensity intensity intensity
                                                      Nothing -> renderObj ray rest bgColor


uniform01 :: IO Double
uniform01 = uniformRM (0.0, 1.0) globalStdGen

randomNormalV3 :: IO V3D
randomNormalV3 = do 
  a1 <- uniformRM (0.0, 1.0) globalStdGen
  a2 <- uniformRM (0.0, 1.0) globalStdGen
  a3 <- uniformRM (0.0, 1.0) globalStdGen
  return $ signorm $ V3 a1 a2 a3
