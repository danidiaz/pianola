{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Pianola.Geometry (
        Interval,
        inside1d,
        Geometrical (..)
    ) where

import Prelude hiding (catch,(.),id)
import Data.MessagePack
import Data.Attoparsec.ByteString
import qualified Data.Text as T
import qualified Data.Iteratee as I
import qualified Data.Attoparsec.Iteratee as AI
import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Identity
import Data.Functor.Compose
import Control.Category
import Control.Error
import Control.Monad
import Control.Applicative
import Control.Monad.Base
import Control.Monad.Trans.Class
import Control.Monad.Free

import Pianola.Util

type Point1d = Int
type Interval = (Int,Int)

inside1d :: Interval -> Point1d -> Bool
inside1d (x1,x2) u = x1 <= u && u <= x2

type Point2d = (Int,Int)
type Dimensions2d = (Int,Int)

--liesWithin :: Interval -> Interval -> Bool
--liesWithin (u1,v1) (u2,v2) = (u1 <= u2) && (v1 >= v2)

class Geometrical g where
    nwcorner :: g -> Point2d

    dimensions :: g -> Dimensions2d

    width :: g -> Int
    width = fst . dimensions 

    height :: g -> Int
    height = snd . dimensions 

    minx :: g -> Int
    minx = fst . nwcorner
    
    midx :: g -> Int
    midx g = minx g + div (width g) 2

    maxx :: g -> Int
    maxx g = (fst $ nwcorner g) + (fst $ dimensions g)

    xband :: g -> Interval
    xband g = (minx g,maxx g)
    
    miny :: g -> Int
    miny = snd . nwcorner

    midy :: g -> Int
    midy g = miny g + div (height g) 2

    maxy :: g -> Int
    maxy g = (snd $ nwcorner g) + (snd $ dimensions g)

    yband :: g -> Interval
    yband g = (miny g,maxy g)

    area :: g -> Int
    area g = width g * height g

    midpoint :: g -> Point2d
    midpoint g = (midx g, midy g)
