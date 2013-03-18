{-# LANGUAGE FlexibleInstances #-}

module Pianola.Geometry (
        Interval,
        Point1d,
        inside1d,
        before1d,
        after1d,
        Point2d,
        Dimensions2d,
        mid,        
        Geometrical (..),
        sameLevelRightOf 
    ) where

import Prelude hiding (catch,(.),id)
import Control.Category

type Interval = (Int,Int)

type Point1d = Int

inside1d :: Interval -> Point1d -> Bool
inside1d (x1,x2) u = x1 <= u && u <= x2

before1d :: Interval -> Point1d -> Bool
before1d (x1,_) x = x <= x1

after1d :: Interval -> Point1d -> Bool
after1d (_,x2) x = x2 <= x

-- | (x,y)
type Point2d = (Int,Int)

-- | (width,height)
type Dimensions2d = (Int,Int)

mid :: Interval -> Point1d
mid (x1,x2) = div (x1+x2) 2

-- | Class of objects with rectangular shape and located in a two-dimensional
-- plane.
class Geometrical g where
    -- | Position of the north-west corner.
    nwcorner :: g -> Point2d

    dimensions :: g -> Dimensions2d

    width :: g -> Int
    width = fst . dimensions

    height :: g -> Int
    height = snd . dimensions

    minX :: g -> Int
    minX = fst . nwcorner

    midX :: g -> Int
    midX = mid . yband

    minY :: g -> Int
    minY = snd . nwcorner

    midY :: g -> Int
    midY = mid . yband

    xband :: g -> Interval
    xband g = 
        let gminX = minX g
        in (gminX, gminX + (fst . dimensions) g)
    
    yband :: g -> Interval
    yband g = 
        let gminY = minY g
        in (gminY, gminY + (snd . dimensions) g)

    area :: g -> Int
    area g = width g * height g

    midpoint :: g -> Point2d
    midpoint g = (midX g, midY g)

-- | True if the second object is roughly at the same height and to the right
-- of the first object.
sameLevelRightOf :: (Geometrical g1, Geometrical g2) => g1 -> g2 -> Bool
sameLevelRightOf ref c =
    inside1d (yband c) (midY ref) && after1d (xband ref) (minX c)
