module Geometry (
    moveTo,
    toRadians,
    toDegrees,
    Point2D(..),
    Robot(..),
    Part(..),
    Segment(..),
    Joint(..)
    ) where


class Point a where
    distance :: a -> a -> Float
    
-- a two-dimensional point for use in calculations
data Point2D = Point2D {
        px :: Float
    ,   py :: Float
    } deriving Show

instance Point Point2D where
    distance p1 p2 =    let
                            a = px p2 - px p1
                            b = py p2 - py p1
                        in  sqrt $ a*a + b*b

-- the below data types aren't really used for much now...
-- but allowing this simple configuration and depending on it lets
-- replace any part of a hand without looking deep into the code

-- Segment is a part which connects two joints
data Segment = Segment {
    len :: Float
    }

data Joint = Joint {
    range :: Int,
    step :: Float
    }

data Part = Part { segment :: Segment, joint :: Joint }

data Robot = Robot Part Part

-- first one is side for desired angle
angle :: Float -> Float -> Float -> Float
angle a b c = toDegrees $ acos $ (b*b + c*c - a*a) / (2 * b * c)

moveTo :: Robot -> Point2D -> [Float]
moveTo (Robot first second) pos =
    let a' = len $ segment second
        b' = distance (Point2D 0 0) pos
        c' = len $ segment first
        a  = angle (py pos) b' (px pos) + angle a' b' c'
        b  = angle b' a' c'
    in [b, a]
    
toRadians :: Float -> Float
toRadians n = n * pi / 180

toDegrees :: Float -> Float
toDegrees n = n * 180 / pi