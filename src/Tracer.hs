module Tracer where

import Surface
import Color
import Lighting
import Ray

import Data.Colour

-- cap on ray tracing recursion depth
traceDepth :: Int
traceDepth = 4

{-
 - recursive ray tracing function.
 - if a given ray hits a surface in the scene:
 - blend the reflectecd color of the interesecting point on the surface with
 - the overall color from lighting. Reflected color is the resulting color
 - from recursively tracing a reflected ray from the point on the surface.
 - otherwise, just return the bg color.
-}
trace :: Ray        -- ray to be traced
      -> [Surface]  -- list of all surfaces
      -> [Light]    -- list of all lights
      -> Int      -- limits recursion depth
      -> Color      -- resulting color from trace
trace _ _ _ 0                = bgColor
trace ray scene lights limit =
    case closestIntersection ray scene of
        Nothing -> bgColor
        Just (surface, pt) -> blend (reflection surface) reflectedColor lighting
            where
                n = shapeNormal (shape surface) pt
                outRay = outgoingRay ray pt n
                reflectedColor = trace outRay scene lights (limit-1)
                lighting = lightingColor surface scene pt lights
