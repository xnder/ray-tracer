module Surface where

import Prelude hiding ((<*>))

import Vector
import Color
import Ray


data Shape = Sphere {center :: Point, radius :: Radius} -- | Plane Point Direction
    deriving Show

data Surface = Surface {shape :: Shape, color :: Color, absorption :: Double, reflection :: Double}
    deriving Show

type Radius = Scalar

-- closest surface intersection on the ray's path
closestIntersection :: Ray -> [Surface] -> Maybe (Surface, Point)
closestIntersection ray surfaces     = foldl (closestIntersection' . origin $ ray) Nothing (map (surfaceIntersection ray) surfaces)

closestIntersection' :: Point -> Maybe (Surface, Point) -> Maybe (Surface, Point) -> Maybe (Surface, Point)
closestIntersection' pt Nothing Nothing = Nothing
closestIntersection' pt Nothing x = x
closestIntersection' pt x Nothing = x
closestIntersection' pt (Just (s, xpt)) (Just (s', xpt'))
    | distance xpt < distance xpt' = Just (s, xpt)
    | otherwise = Just (s', xpt')
        where distance pt' = magnitude $ vectorFromPoints pt pt'


surfaceIntersection :: Ray -> Surface -> Maybe (Surface, Point)
surfaceIntersection ray surface = fmap (\pt -> (surface, pt)) (shapeIntersection ray (shape surface))

{-
 - finds the point of intersecton of the line along the ray and the given
 - shape if there is one.
 -}
shapeIntersection :: Ray -> Shape -> Maybe Point

-- intersecting a sphere
shapeIntersection (Ray eye dir) (Sphere center radius)
    | cp' > radius      = Nothing                  -- the ray is clearly too far from the sphere
--  | c' == radius      = Just eye                 -- eye is at the sphere's surface
    | c' < radius       = Just (eye <.> (p <+> x)) -- eye inside sphere
    | (c <*> dir) > 0   = Just (eye <.> (p <-> x)) -- sphere's center is ahead of eye
    | otherwise         = Nothing
    where
        c   = (V center) <-> (V eye)  -- from eye to center
        c'  = magnitude c
        p   = (c <*> dir) <#> dir     -- projected vector of c onto ray
        cp' = magnitude (p <-> c)     -- length of opposite side of triangle ecp
        x'  = sqrt $ radius^2 - cp'^2 -- distance along ray from p to intersecting point
        x   = x' <#> dir              -- from eye to point of intersection

-- normal vector at a point on a shape
shapeNormal :: Shape -> Point -> Direction
shapeNormal c@(Sphere center radius) pt = normalize $ vectorFromPoints center pt
