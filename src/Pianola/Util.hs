{-# LANGUAGE FlexibleInstances #-}

module Pianola.Util (
        replusify,
        tomaybet,
        Treeish(..),
        Produ,
        Consu,
        Loggy(..),
        LogEntry(..),
        Image,
        Nullipotent(runNullipotent),
        Tag,
        Sealed(tags,unseal),
        addTag
    ) where

import Prelude hiding (catch,(.),id)
import Control.Category
import Data.Tree
import Data.MessagePack
import Data.Attoparsec.ByteString
import Control.Monad
import Control.Comonad
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env    
import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Logic
import Control.Proxy
import qualified Data.Text as T
import qualified Data.ByteString as B

import Pianola.Internal

replusify:: MonadPlus m => [a] -> m a
replusify = msum . map return

tomaybet:: Monad m => LogicT m a -> MaybeT m a
tomaybet = MaybeT . liftM replusify . observeManyT 1

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


-- logging
type Image = B.ByteString

class Functor l => Loggy l where
    logentry::LogEntry -> l ()

    logmsg::T.Text -> l ()
    logmsg = logentry . TextEntry

    logimg::Image -> l ()
    logimg = logentry . ImageEntry

    logmsgK::T.Text -> a -> l a
    logmsgK msg = (<$ logmsg msg) 

data LogEntry = TextEntry T.Text 
                |ImageEntry Image

-- pipes
type Produ t = Producer ProxyFast t
type Consu t = Consumer ProxyFast t

instance Monad m => Loggy (Produ LogEntry m) where
    logentry = respond 

instance (Monad l, Loggy l) => Loggy (LogicT l) where
    logentry = lift . logentry

instance (Monad l, Loggy l) => Loggy (MaybeT l) where
    logentry = lift . logentry

