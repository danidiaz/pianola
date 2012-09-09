{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}

module Xanela.Util (
        mapFreeT,
        mplusify,
        treeflat,
        forestflat,
        unitK,
        composeKs,
        prefixK,
        prefixK_,
        sandwichK,
        sandwichK_
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
unitK _ = return ()

-- This function was copied from here:
-- http://stackoverflow.com/questions/8716668/folding-flatmap-bind-over-a-list-of-functions-a-k-a-name-that-combinator
composeKs :: Monad m => [a -> m a] -> a -> m a
composeKs  = foldr (>=>) return

prefixK:: Monad m => (a -> m b) -> ((a -> m b) -> [b -> m a]) -> a -> m a 
prefixK prefix kl = 
    let kl' = map ((>=>) prefix) (kl prefix)
    in composeKs kl'

prefixK_ f1 l = prefixK f1 (\_->l)

sandwichK:: Monad m => (a -> m b) -> (c -> m a) -> ((a -> m b) -> (c -> m a) -> [b -> m c]) -> a -> m a 
sandwichK prefix suffix kl = 
    let kl' = map ((>=>) prefix) (kl prefix suffix)
        kl'' = map ((<=<) suffix) kl' 
    in composeKs kl''

sandwichK_ f1 f2 l = sandwichK f1 f2 (\_ _->l)

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
