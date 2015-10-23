module Main where

import Data.Array.Repa
import Data.Array.Repa.IO.BMP
import Data.Functor.Identity

import Data.Colour.RGBSpace.HSV
import Data.Colour.SRGB

import Data.Word

import Tracer
import Camera
import Pixel
import Ray
import Scene

{-

TODO:
    add ambient lighting
    add plane surfaces
    testing
    use Data.Vector or repa vector
    adjust lighting
    improvments:
        surface bumpiness / texture
-}

main :: IO ()
main = runTracer xDim yDim evalPixel

runTracer :: Int -> Int -> (Int -> Int -> (Word8, Word8, Word8)) -> IO ()
runTracer xDim yDim f = writeImageToBMP "out.bmp" . runIdentity . computeP $ fromFunction (ix2 xDim yDim) (\(Z:.x:.y) -> f x y)

{-
 - gives the color of the specified pixel resulting from tracing the pixel's
 - corresponding camera ray
 -}
evalPixel :: Int -> Int -> (Word8, Word8, Word8)
evalPixel x y = (channelRed color, channelGreen color, channelBlue color)
    where
        ray = pixelRay (Pixel x y) camera
        color = Data.Colour.SRGB.toSRGB24 $ trace ray scene lights traceDepth
