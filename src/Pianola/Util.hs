{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Pianola.Util (
        eq,
        eqm,
        replusify,
        treeflat,
        treeflat',
        forestflat,
        composeK,
        prependK,
        appendK,
        threadKs,
        narrow,
        narrowK,
        lo,
        PianolaLog(..),
        LogEntry(..),
        Image,
        LogConsumer,
        LogProducer,
        Nullipotent(..),
        Sealed(..)
    ) where

import Prelude hiding (catch,(.),id)
import Control.Category
import Data.Foldable (Foldable)
import Data.Traversable
import Data.Tree
import Data.List
import Data.MessagePack
import Data.Attoparsec.ByteString
import Control.Monad
import Control.Comonad
import Control.Applicative
import Control.Monad.Base
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Maybe
import Control.Monad.Logic hiding (observe)
import Control.Monad.Free
import Control.MFunctor
import Control.Proxy -- (Producer,Consumer,ProxyFast, respond, fromListS,>->)
import qualified Data.Text as T
import qualified Data.ByteString as B

--
eq :: Eq a => ((a->Bool) -> b) -> a -> b
eq f a = f (==a)

eqm :: Eq a => ([a->Bool] -> b) -> [a] -> b
eqm f a = f $ map (==) a

-- Kleisie

-- This function was copied from here:
-- http://stackoverflow.com/questions/8716668/folding-flatmap-bind-over-a-list-of-functions-a-k-a-name-that-combinator
composeK :: Monad m => [a -> m a] -> a -> m a
composeK  = foldr (>=>) return

prependK:: (Monad m) => (a -> m b) -> [b -> m c] -> [a -> m c]
prependK prefix = map $ (>=>) prefix

appendK:: (Monad m) => (b -> m c) -> [a -> m b] -> [a -> m c]
appendK suffix = map $ (<=<) suffix

threadKs :: (Monad m,Monad n) => (a -> m a) -> ((a -> n a) -> a -> m a) -> (a -> n b, c -> n a) -> [b -> n c] -> a -> m a
threadKs separatork ktransformer (prefix,suffix) =
    composeK . intersperse separatork . map ktransformer . prependK prefix . appendK suffix 

-- logic helpers

replusify:: MonadPlus m => [a] -> m a
replusify = msum . map return

narrow:: Monad m => LogicT m a -> MaybeT m a
narrow = MaybeT . liftM replusify . observeManyT 1

narrowK :: Monad m => (a -> LogicT m b) -> a -> MaybeT m b 
narrowK = fmap narrow 

lo :: Monad m => (a -> LogicT m b) -> a -> MaybeT m b 
lo = narrowK

treeflat:: MonadPlus m => Tree a -> m a
treeflat = replusify . flatten 

treeflat':: MonadPlus m => Tree a -> m (Tree a)
treeflat' = replusify . flatten . duplicate

forestflat:: MonadPlus m => Forest a -> m a
forestflat forest = replusify forest >>= treeflat 

-- useful msgpack instances
instance (Unpackable a, Unpackable b) => Unpackable (Either a b) where
    get = do
        tag <- get::Parser Int
        case tag of
            1 -> Left <$> get
            0 -> Right <$> get

instance Unpackable a => Unpackable (Tree a) where
    get = Node <$> get <*> get

-- useful MonadBase instances

instance MonadBase b m => MonadBase b (ProxyFast x y u v m) where
    liftBase = lift.liftBase

instance MonadBase b m => MonadBase b (LogicT m) where
    liftBase = lift.liftBase

-- logging
type Image = B.ByteString

class Functor l => PianolaLog l where
    xanlog::LogEntry -> l ()

    logmsg::T.Text -> l ()
    logmsg = xanlog . TextEntry

    logimg::Image -> l ()
    logimg = xanlog . ImageEntry

    logmsgK::T.Text -> a -> l a
    logmsgK msg = (<$ logmsg msg) 

data LogEntry = TextEntry T.Text 
                |ImageEntry Image

type LogProducer m = Producer ProxyFast LogEntry m
type LogConsumer m = Consumer ProxyFast LogEntry m

instance Monad m => PianolaLog (LogProducer m) where
    xanlog = respond 

--instance (Monad l, PianolaLog l) => PianolaLog (Producer ProxyFast r l) where
--    xanlog = lift . xanlog

instance (Monad l, PianolaLog l) => PianolaLog (LogicT l) where
    xanlog = lift . xanlog

instance (Monad l, PianolaLog l) => PianolaLog (MaybeT l) where
    xanlog = lift . xanlog

-- 
newtype Nullipotent m a = Nullipotent { runNullipotent:: m a }
   deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)
   
instance (Monad m) => Monad (Nullipotent m) where
   (Nullipotent m) >>= k = Nullipotent $ m >>= (runNullipotent . k)
   return = Nullipotent . return
   
data Sealed m = Sealed {
       tags:: [T.Text],
       unseal:: m ()
   }
   
