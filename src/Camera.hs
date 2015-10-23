module Camera where

import Vector

data Camera = Camera {originCorner :: Point, eye :: Point, xDir :: Direction, yDir :: Direction }
    deriving Show

xDim :: Int
xDim = 200

yDim :: Int
yDim = 200

zDim :: Int
zDim = 140

testCamera = Camera [0, 4, 0] [2,2,4] (V [1,0,0]) (V [0,-1,0])

{-
 - camera orientation is based on the image as a the base of the pyramid
 - and the eye as the top corner.
 - the X-Y direction is based on the top right corner of the image
 - Z direction points back to eye from the center of the image (base of pyramid)
-}
cameraXDir = (V [1, 0, 0]) -- X direction of the corner of the image
cameraYDir = (V [0, 1, 0]) -- Y direction of the corner of the image
cameraZDir = (V [0, 0, 1]) -- direction from the center to the eye

camera = camera' [0, 0, 50] (fromIntegral xDim) (fromIntegral yDim) (fromIntegral zDim) cameraXDir cameraYDir cameraZDir
--Camera [0, 100, 50] [50, 50, 100] cameraXDir cameraYDir

{-
 - positions a camera based on the height, base width and length
 - length of the view's pyramid, and camera direction vectors.
-}
camera' :: Point -> Double -> Double -> Double -> Vector -> Vector -> Vector -> Camera
camera' corner@(x:y:z:[]) w h d xdir ydir zdir = Camera corner eye xdir ydir
    where
        center = ((w / 2) <#> xdir) <+> ((h / 2) <#> ydir) <+> (d <#> zdir) --[x + w / 2, y + h / 2, z + d]
        eye    = pointFromVector corner center
