{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Pianola.Geometry (
        Interval,
        liesWithin,
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

type Interval = (Int,Int)

liesWithin :: Interval -> Interval -> Bool
liesWithin (u1,v1) (u2,v2) = (u1 <= u2) && (v1 >= v2)

class Geometrical g where
    nwcorner :: g -> (Int,Int)

    dimensions :: g -> (Int,Int)

    width :: g -> Int
    width = fst . dimensions 

    height :: g -> Int
    height = snd . dimensions 

    minx :: g -> Int
    minx = fst . nwcorner
    
    maxx :: g -> Int
    maxx g = (fst $ nwcorner g) + (fst $ dimensions g)

    xband :: g -> Interval
    xband g = (minx g,maxx g)
    
    miny :: g -> Int
    miny = snd . nwcorner

    maxy :: g -> Int
    maxy g = (snd $ nwcorner g) + (snd $ dimensions g)

    yband :: g -> Interval
    yband g = (miny g,maxy g)

    area :: g -> Int
    area g = width g * height g

    midx :: g -> Int
    midx g = minx g + div (width g) 2

    midy :: g -> Int
    midy g = miny g + div (height g) 2

    midpoint :: g -> (Int,Int)
    midpoint g = (midx g, midy g)
