{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}

module Xanela.Util (
        mapFreeT,
        mplusify,
        treeflat,
        forestflat,
        composeK,
        prependK,
        sandwichK
    ) where

import Prelude hiding (catch,(.),id)
import Control.Category
import Data.Tree
import Data.MessagePack
import Data.Attoparsec.ByteString
import Control.Monad
import Control.Applicative
import Control.Monad.Base
import "transformers-free" Control.Monad.Trans.Free

-- Kleisie

-- This function was copied from here:
-- http://stackoverflow.com/questions/8716668/folding-flatmap-bind-over-a-list-of-functions-a-k-a-name-that-combinator
composeK :: Monad m => [a -> m a] -> a -> m a
composeK  = foldr (>=>) return

prependK:: Monad m => (a -> m b) -> [b -> m a] -> a -> m a 
prependK prefix kl = 
    let kl' = map ((>=>) prefix) kl
    in composeK kl'

sandwichK:: Monad m => (a -> m b) -> [b -> m c] -> (c -> m a) -> a -> m a 
sandwichK prefix kl suffix = 
    let kl' = map ((>=>) prefix) kl
        kl'' = map ((<=<) suffix) kl' 
    in composeK kl''

-- this may come in handy to map pipes
mapFreeT::  (Functor f, Monad m, Monad m') => (forall a. m a -> m' a) -> FreeT f m a -> FreeT f m' a
mapFreeT fm (FreeT m) = 
    let mapFreeF (Free f) = Free $ fmap (mapFreeT fm) f
        mapFreeF (Pure a) = Pure a
    in FreeT . fm . liftM mapFreeF $ m

-- logic helpers
mplusify :: MonadPlus m => [a] -> m a
mplusify = msum . map return

treeflat:: MonadPlus m => Tree a -> m a
treeflat = mplusify . flatten 

forestflat:: MonadPlus m => Forest a -> m a
forestflat forest = mplusify forest >>= treeflat 

-- useful msgpack instances
instance (Unpackable a, Unpackable b) => Unpackable (Either a b) where
    get = do
        tag <- get::Parser Int
        case tag of
            1 -> do
                v0 <- get
                return $ Left v0
            0 -> do 
                v1 <- get
                return $ Right v1

instance Unpackable a => Unpackable (Tree a) where
    get = do
        v1 <- get
        v2 <- get
        return (Node v1 v2)
