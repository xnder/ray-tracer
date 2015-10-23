module Lighting where

import Prelude hiding ((<*>))

import Surface
import Vector
import Color
import Ray

import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB

data Light = PointLight Point Color | Ambient Color
    deriving Show

lightRayBackoff = 0.001
dimnessFactor = 1.0

testLight1 = PointLight [-1, 1] blue
testLight2 = PointLight [3, 3] green
testLights = [testLight1, testLight2]


-- applies a function to the first item in a pair
first :: (a -> a') -> (a, b) -> (a', b)
first f (a, b) = (f a, b)

-- sums colors of all lights at the point on the surface
lightingColor :: Surface -> [Surface] -> Point -> [Light] -> Color
lightingColor _ _ _ [] = bgColor
lightingColor surface scene pt lights = combined --color surface --affineCombo colors (color surface)
    where
        colors'  = map (lightColor scene pt surface) lights
        nColors  = fromIntegral $ length colors'
        wColors  = map (\ c -> (1.0/nColors, c)) colors'
        combined = affineCombo (tail wColors) (snd . head $ wColors)
        --totalW  = sum $ map fst colors'
        --colors  = map (first $ \f -> (absorption surface) * f / totalW) colors'


lightBlocked :: Maybe (Surface, Point) -> Point -> Vector -> Bool
lightBlocked Nothing _ _          = False
lightBlocked (Just (_, xpt)) pt v = (x <*> x) < (v <*> v)
    where
        x = vectorFromPoints pt xpt

lightColor :: [Surface] -> Point -> Surface -> Light -> Color

lightColor scene surfacePt surface (PointLight ptLightPos ptLightColor)
    | lightBlocked block lPt v = bgColor
    | otherwise                = blend (absorption surface) color' (color surface)
    where
        n      = shapeNormal (shape surface) surfacePt         -- normal to surface
        v      = vectorFromPoints surfacePt ptLightPos         -- vector from point to light
        lDir   = normalize v                                   -- dir to light
        lPt    = pointFromVector surfacePt $ lightRayBackoff <#> lDir -- move light start pt away from surface
        block  = closestIntersection (Ray lPt lDir) scene      -- surface that may block light
        factor = (lDir <*> n)                                  -- factor to blend with bgColor
        color' = blend factor ptLightColor bgColor             -- final blended color for light on surface point
