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
        replusify,
        tomaybet,
        maybify,
        Treeish(..),
        Prod,
        Loggy(..),
        LogEntry(..),
        Image,
        LogConsumer,
        LogProducer,
        Nullipotent(runNullipotent),
        Tag,
        Sealed(tags,unseal),
        addTag
    ) where

import Prelude hiding (catch,(.),id)
import Control.Category
import Data.Foldable (Foldable)
import Data.Traversable
import Data.Tree
import Data.List
import Data.MessagePack
import Data.Attoparsec.ByteString
import Control.Error.Safe
import Control.Monad
import Control.Comonad
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env    
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

import Pianola.Internal

replusify:: MonadPlus m => [a] -> m a
replusify = msum . map return

tomaybet:: Monad m => LogicT m a -> MaybeT m a
tomaybet = MaybeT . liftM replusify . observeManyT 1

maybify :: Monad m => LogicT m a -> LogicT m (Maybe a)
maybify l = lift $ observeManyT 1 l >>= return . replusify

class Treeish l where
    children :: MonadPlus m => l -> m l
    descendants :: MonadPlus m => l -> m l

instance Treeish (Tree a) where
    children = replusify . subForest
    descendants = replusify . flatten . duplicate

instance Treeish (EnvT e Tree a) where
    children  = replusify . map rootLabel . subForest . lower . duplicate
    descendants = replusify . flatten . lower . duplicate

-- useful msgpack instances
instance (Unpackable a, Unpackable b) => Unpackable (Either a b) where
    get = do
        tag <- get::Parser Int
        case tag of
            1 -> Left <$> get
            0 -> Right <$> get

instance Unpackable a => Unpackable (Tree a) where
    get = Node <$> get <*> get

-- pipes
type Prod t = Producer ProxyFast t

-- logging
type Image = B.ByteString

class Functor l => Loggy l where
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

instance Monad m => Loggy (LogProducer m) where
    xanlog = respond 

instance (Monad l, Loggy l) => Loggy (LogicT l) where
    xanlog = lift . xanlog

instance (Monad l, Loggy l) => Loggy (MaybeT l) where
    xanlog = lift . xanlog

-- 
--

