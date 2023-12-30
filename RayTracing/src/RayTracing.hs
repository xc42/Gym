module RayTracing  where

import Linear.Affine
import Linear.V3
import Linear.Vector ( (^*), (*^) )
import Linear.Metric (dot)
import Graphics.Image as Img

type V3f = V3 Float
type Point3f = V3 Float

data Ray = Ray {oringin :: Point3f,direction :: V3f}

data Hit = Hit {hitPoint :: Point3f, normal :: V3f, frontHit :: Bool}
    deriving (Show)


class Object o where
    hitTest :: Ray -> o -> Maybe Hit


data Sphere = Sphere { center :: Point3f, radius :: Float }

instance Object Sphere where
    hitTest (Ray rA rb) (Sphere sC r) =
        let a = rb `dot` rb
            b = rb `dot` (rA - sC)
            c = (rA - sC) `dot` (rA - sC) - r * r
            det =  b * b - a * c
        in if det < 0 
              then Nothing 
              else let t = (-b + sqrt det) / a
                       hp = rA + t *^ rb
                    in Just $ Hit hp 0.0 True


renderWorld :: (Object o) => Point3f -> Float -> Int -> Int -> Float -> o -> Image VS RGB Double
renderWorld camera aspectRatio vpW imgW focal obj = 
    let vpH = fromIntegral vpW / aspectRatio
        imgH = fromIntegral imgW / aspectRatio
        pixelO = camera - 0.5 * V3 (fromIntegral vpW) vpH 0.0 + V3 0.0 0.0 focal
        scaleRatio = vpH / imgH :: Float
        toWorldCoord i j = pixelO + scaleRatio *^ fmap fromIntegral (V3 i (-j) 0)
        renderPixel (row, col) = let coord = toWorldCoord col row
                                     ray  = Ray camera (coord - camera)
                            in case hitTest ray obj of
                                 Just (Hit p norm front) -> PixelRGB 0.0 1.0 0.0
                                 Nothing -> PixelRGB 0.0 0.0 (fromIntegral row) / fromIntegral imgW
    in 
        makeImageR VS (round imgH, imgW) renderPixel


