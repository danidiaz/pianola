{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Xanela.Util (
        mapFreeT,
        mplusify,
        treeflat,
        forestflat
    ) where

import Prelude hiding (catch,(.))
import Control.Category
import Data.Tree
import Data.MessagePack
import Data.Attoparsec.ByteString
import Control.Monad
import Control.Applicative
import Control.Monad.Base
import Control.Monad.Trans.Free

-- this may come in handy to map pipes
mapFreeT::  (Functor f, Functor m) => (forall a. m a -> m' a) -> FreeT f m a -> FreeT f m' a
mapFreeT f (FreeT m) = FreeT (f ((fmap.fmap) (mapFreeT f) m))

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
