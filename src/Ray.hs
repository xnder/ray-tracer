module Ray where

import Camera
import Vector
import Pixel

testRay1 = Ray [0,1] $ normalize $ V [1,0]
testRay2 = Ray [6,1] $ normalize $ V [-1,0]
testRay3 = Ray [6,1] $ normalize $ V [0,1]

data Ray = Ray {origin :: Point, dir :: Direction}
    deriving Show

{-
 - creates a ray for a specified pixel depending on the camera's orientation
 -}
pixelRay :: Pixel -> Camera -> Ray
pixelRay p (Camera topright eye xdir ydir) = Ray eye n
    where
        vx = (fromIntegral (xpos p)) <#> xdir
        vy = (fromIntegral (ypos p)) <#> ydir
        pt = pointFromVector topright (vx <+> vy)
        n  = normalize (vectorFromPoints eye pt)

-- computes the outgoing Ray from an incoming Ray and Normal Direction at a point
outgoingRay :: Ray -> Point -> Direction -> Ray
outgoingRay ray pt n = Ray pt outdir
    where
        indir  = (dir ray)
        outdir = indir <-> (2 <#> n)
