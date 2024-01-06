module RayTracing  where

import Control.Monad
import Linear.Affine
import Linear.V3
import Linear.Vector ( (^*), (*^))
import Linear.Metric 

import Graphics.Image as Img hiding (normalize, sum)
import Graphics.Image.Interface (makeImageM)

import System.Random.Stateful
import Debug.Trace
import Text.Printf

type V3D = V3 Double
type Point3f = V3 Double

data Ray = Ray {oringin :: Point3f,direction :: V3D}
    deriving (Show)

data Hit = Hit {hitPoint :: Point3f, normal :: V3D, frontHit :: Bool}
    deriving (Show)



data Object = Sphere { center :: Point3f, radius :: Double, material :: Material}
    deriving (Show)

data Material = Lambertian V3D | Metal V3D
    deriving (Show)


hitTests :: Ray -> [Object] -> Maybe (Hit, Object)
hitTests _ [] = Nothing
hitTests ray@(Ray r0 dir0) (obj:rest) =
    let hit0 = hitTest ray obj
        hit1 = hitTests ray rest
     in case (hit0, hit1) of
          (Nothing, Nothing) -> Nothing
          (Just h0, Nothing) -> Just (h0, obj)
          (Nothing, Just _) -> hit1
          (Just h0@(Hit hp0 _ _), Just (Hit hp1 _ _, _)) -> 
              let l0 = (hp0 - r0) `dot` (hp0 - r0)
                  l1 = (hp1 - r0) `dot` (hp1 - r0)
               in if l0 <= l1 then Just (h0, obj) else hit1
    where hitTest (Ray rA rb) obj@(Sphere sC r _) =
            let a = rb `dot` rb
                b = rb `dot` (rA - sC)
                c = (rA - sC) `dot` (rA - sC) - r * r
                disc =  b * b - a * c
                t = (-b - sqrt disc) / a
                hp = rA + (t *^ rb)
             in if disc < 0 || t <= 0
                   then Nothing 
                   else Just $ Hit hp (signorm (hp - sC)) True


scatter :: Ray -> Hit -> Material -> IO (Ray, V3D)
scatter (Ray _ dir) (Hit hp nm _) (Metal albedo) = return (Ray hp (dir - 2.0 * (dir `dot` nm) *^ nm), albedo)
scatter (Ray _ _) (Hit hp nm _) (Lambertian albedo) = randomNormalV3 >>= \randDir -> return (Ray hp (randDir + nm), albedo)

getObjMaterial :: Object -> Material
getObjMaterial (Sphere _ _ m) = m

renderWorld :: Point3f -> Double -> Int -> Int -> Double -> [Object] -> IO (Image VS RGB Double)
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
            -- let bgColor = V3 (0.6*ratio) (0.8*ratio) (1.0*ratio)
            let bgColor = V3 1.0 1.0 1.0
            let maxBounce = 50
            colors <- sequence [renderObj r objs maxBounce bgColor| r <- rays]
            let V3 r g b = (1.0 / fromIntegral (length colors)) *^ sum colors
            return $ PixelRGB r g b
    in  makeImageM (round imgH, imgW) renderPixel
    where 
        renderObj :: Ray -> [Object] -> Int -> V3D -> IO V3D
        renderObj ray@(Ray origin dir) objs bounce bgColor
          | bounce == 0 = return $ V3 0.0 0.0 0.0
          | otherwise = case hitTests ray objs of
                          Just (hit, obj) ->  do 
                              (ray', attenuation) <- scatter ray hit (getObjMaterial obj)
                              fmap (* attenuation) (renderObj ray' objs (bounce - 1) bgColor)
                          Nothing -> return bgColor


uniform01 :: IO Double
uniform01 = uniformRM (0.0, 1.0) globalStdGen

randomNormalV3 :: IO V3D
randomNormalV3 = do 
  a1 <- uniformRM (0.0, 1.0) globalStdGen
  a2 <- uniformRM (0.0, 1.0) globalStdGen
  a3 <- uniformRM (0.0, 1.0) globalStdGen
  return $ signorm $ V3 a1 a2 a3
