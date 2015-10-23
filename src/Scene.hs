module Scene where

import Surface
import Lighting

import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB


testSphere1 = Sphere [2,1] 1
testSphere2 = Sphere [4,1] 1
testSurface1 = Surface testSphere1 red 0.5 0.1
testSurface2 = Surface testSphere2 blue 0.5 0.9
testSurfaces = testSurface1 : testSurface2 : []

redball   = Surface (Sphere [100, 100, -200] 100) red   0.1 0.0
greenball = Surface (Sphere [-50,   0,   -100] 100) green 0.1 0.0
light1 = PointLight [100, 150, 0] white
light2 = PointLight [150, 150, 0] white
scene = [redball, greenball]
lights = [light1, light2]
