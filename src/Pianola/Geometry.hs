{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Data.List

import Pianola.Util

type Interval = (Int,Int)

type Point1d = Int

inside1d :: Interval -> Point1d -> Bool
inside1d (x1,x2) u = x1 <= u && u <= x2

before1d :: Interval -> Point1d -> Bool
before1d (x1,_) x = x <= x1

after1d :: Interval -> Point1d -> Bool
after1d (_,x2) x = x2 <= x

type Point2d = (Int,Int)

type Dimensions2d = (Int,Int)

mid :: Interval -> Point1d
mid (x1,x2) = div (x1+x2) 2

class Geometrical g where
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

    areacmp :: g -> g -> Ordering
    areacmp g1 g2 = compare (area g1) (area g2)

    midpoint :: g -> Point2d
    midpoint g = (midX g, midY g)

sameLevelRightOf :: (Geometrical g1, Geometrical g2) => g1 -> g2 -> Bool
sameLevelRightOf ref c =
    inside1d (yband c) (midY ref) && after1d (xband ref) (minX c)
