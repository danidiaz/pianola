{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Xanela.Util (
        replusify,
        treeflat,
        forestflat,
        composeK,
        prependK,
        appendK,
        sandwich,
        maybeify,
        maybeifyK,
        maybeifyManyK
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

-- Kleisie

-- This function was copied from here:
-- http://stackoverflow.com/questions/8716668/folding-flatmap-bind-over-a-list-of-functions-a-k-a-name-that-combinator
composeK :: Monad m => [a -> m a] -> a -> m a
composeK  = foldr (>=>) return

prependK:: (Monad m) => (a -> m b) -> [b -> m c] -> [a -> m c]
prependK prefix = map $ (>=>) prefix

appendK:: (Monad m) => (b -> m c) -> [a -> m b] -> [a -> m c]
appendK suffix = map $ (<=<) suffix

sandwich:: (Monad m) => (a -> m b) -> (c -> m d) -> [b -> m c] -> [a -> m d]
sandwich prefix suffix = prependK prefix . appendK suffix

-- logic helpers

replusify:: MonadPlus m => [a] -> m a
replusify = msum . map return

maybeify:: Monad m => LogicT m a -> MaybeT m a
maybeify = MaybeT . liftM replusify . observeManyT 1

maybeifyK :: Monad m => (a -> LogicT m b) -> a -> MaybeT m b 
maybeifyK = fmap maybeify 

maybeifyManyK :: Monad m => (a -> LogicT m b) -> (c -> LogicT m a) -> [b -> LogicT m c] -> a -> MaybeT m a
maybeifyManyK prefix suffix = composeK . map maybeifyK . sandwich prefix suffix

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
instance MonadBase b m => MonadBase b (Proxy x y u v m) where
    liftBase = lift.liftBase

instance MonadBase b m => MonadBase b (LogicT m) where
    liftBase = lift.liftBase
