{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Xanela.Util (
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
        XanelaLog(..),
        LogEntry(..),
        Image,
        LogConsumer,
        LogProducer,
        Nullipotent(..),
        Sealed(..),
        ObserverF(..),
        Observer(..),
        focus,
        Pianola(..),
        Delay,
        playPianola
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

class Functor l => XanelaLog l where
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

instance Monad m => XanelaLog (LogProducer m) where
    xanlog = respond 

instance (Monad l, XanelaLog l) => XanelaLog (LogicT l) where
    xanlog = lift . xanlog

instance (Monad l, XanelaLog l) => XanelaLog (MaybeT l) where
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
   
type Pr t = Producer ProxyFast t

type Observation l m a = MaybeT (Pr l (Nullipotent m)) a 

data ObserverF l o a = ObserverF { unObserverF :: forall m. Monad m => o m -> Observation l m a }
   
instance Functor (ObserverF l o) where
   fmap f (ObserverF x) = ObserverF $ (fmap.liftM) f x

type Observer l o = Free (ObserverF l o)

focus :: (forall m. Monad m => o m -> Observation l m (o' m)) -> Observer l o' a -> Observer l o a
focus prefix v =
   let nattrans (ObserverF k) = ObserverF $ prefix >=> k
   in hoistFree nattrans v

runObserver :: Monad m => m (o m) -> Observer l o a -> MaybeT (Pr l m) a
runObserver _ (Pure b) = return b
runObserver mom (Free f) =
   let removeNullipotent = fmap.mapMaybeT $ hoist runNullipotent
   in join $ (lift . lift $ mom) >>= removeNullipotent (unObserverF $ runObserver mom <$> f)

type Delay = Int

type Pianola l' l o m a = Pr (Sealed m) (Pr Delay (MaybeT (Pr l' (Observer l o)))) a 

playPianola :: Monad m => m (o m) -> Pianola l' l o m a -> Pr Delay (MaybeT (Pr l' (MaybeT (Pr l m)))) a
playPianola mom pi =
    let pianola' = hoist (hoist (mapMaybeT (hoist $ runObserver mom))) $ pi 
        injector () = forever $ do
            s <- request ()
            lift . lift . lift . lift . lift . lift $ unseal s -- this should be private
    in runProxy $ const pianola' >-> injector

    




