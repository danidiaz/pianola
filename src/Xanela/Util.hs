{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Xanela.Util (
        mapFreeT,
        replusify,
        treeflat,
        forestflat,
        composeK,
        prependK,
        appendK,
        sandwich,
        maybeify,
        maybeifyK
    ) where

import Prelude hiding (catch,(.),id)
import Control.Category
import Data.Tree
import Data.MessagePack
import Data.Attoparsec.ByteString
import Control.Monad
import Control.Applicative
import Control.Monad.Base
import Control.Monad.Trans.Maybe
import Control.Monad.Logic
import Control.Proxy

import "transformers-free" Control.Monad.Trans.Free

-- Kleisie

-- This function was copied from here:
-- http://stackoverflow.com/questions/8716668/folding-flatmap-bind-over-a-list-of-functions-a-k-a-name-that-combinator
composeK :: Monad m => [a -> m a] -> a -> m a
composeK  = foldr (>=>) return

prependK:: (Monad m) => (a -> m b) -> [b -> m c] -> [a -> m c]
prependK prefix = map $ (>=>) prefix

appendK:: (Monad m) => (b -> m c) -> [a -> m b] -> [a -> m c]
appendK suffix = map $ (<=<) suffix

sandwich:: (Monad m) => (a -> LogicT m b) -> (c -> LogicT m a) -> [b -> LogicT m c] -> a -> MaybeT m a
sandwich prefix suffix = composeK . map maybeifyK . prependK prefix . appendK suffix

-- this may come in handy to map pipes
mapFreeT::  (Functor f, Monad m, Monad m') => (forall a. m a -> m' a) -> FreeT f m a -> FreeT f m' a
mapFreeT fm (FreeT m) = 
    let mapFreeF (Free f) = Free $ fmap (mapFreeT fm) f
        mapFreeF (Pure a) = Pure a
    in FreeT . fm . liftM mapFreeF $ m

-- logic helpers

replusify:: MonadPlus m => [a] -> m a
replusify = msum . map return

maybeify:: Monad m => LogicT m a -> MaybeT m a
maybeify = MaybeT . liftM replusify . observeManyT 1

maybeifyK :: Monad m => (a -> LogicT m a) -> a -> MaybeT m a 
maybeifyK = fmap maybeify 

treeflat:: MonadPlus m => Tree a -> m a
treeflat = replusify . flatten 

forestflat:: MonadPlus m => Forest a -> m a
forestflat forest = replusify forest >>= treeflat 

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

-- useful MonadBase instances
instance MonadBase b m => MonadBase b (Producer l m) where
    liftBase = lift.liftBase

instance MonadBase b m => MonadBase b (LogicT m) where
    liftBase = lift.liftBase
