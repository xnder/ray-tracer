module Vector where

-- TODO:
-- import Data.Vector


newtype Vector = V {unV :: [Double]}
    deriving Show

type Point = [Double]

type Scalar = Double

type Direction = Vector

(<+>) :: Vector -> Vector -> Vector
(V v) <+> (V u) = V $ zipWith (+) v u

(<->) :: Vector -> Vector -> Vector
(V v) <-> (V u) = V $ zipWith (-) v u

(</>) :: Vector -> Scalar -> Vector
(V v) </> a = V $ map (/a) v

(<#>) :: Scalar -> Vector -> Vector
a <#> (V v) = V $ map (a*) v

(<*>) :: Vector -> Vector -> Scalar
(V u) <*> (V v) = sum $ zipWith (*) u v

(<.>) :: Point -> Vector -> Point
pt <.> v = unV $ (V pt) <+> v

magnitude :: Vector -> Scalar
magnitude (V v) = sqrt . sum $ map (^2) v

normalize :: Vector -> Vector
normalize v = v </> (magnitude v)

pointFromVector :: Point -> Vector -> Point
pointFromVector pt v = unV ((V pt) <+> v)

vectorFromPoints :: Point -> Point -> Vector
vectorFromPoints s t = (V t) <-> (V s)

pointsToClose :: Point -> Point -> Bool
pointsToClose x y = (sum $ map abs $ zipWith (-) x y) < 0.01
