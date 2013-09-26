{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Pianola.Util (
        replusify,
        tomaybet,
        Treeish(..),
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
import qualified Data.Text as T
import qualified Data.ByteString as B
import Pipes

import Pianola.Internal

-- | Convenience function to transform a list into any 'MonadPlus'.
replusify:: MonadPlus m => [a] -> m a
replusify = msum . map return

-- | Transforms a zero-or-many result into a zero-or-one result.
tomaybet:: Monad m => LogicT m a -> MaybeT m a
tomaybet = MaybeT . liftM replusify . observeManyT 1

-- | Class of types whose values have children of the same type as themselves.
class Treeish l where
    -- | Direct descendants.
    children :: MonadPlus m => l -> m l
    -- | All direct or indirect descendants, plus the original value.
    descendants :: MonadPlus m => l -> m l

instance Treeish (Tree a) where
    children = replusify . subForest
    descendants = replusify . flatten . duplicate

instance (Comonad c, Treeish (c a)) => Treeish (EnvT e c a) where
    children  a = replusify . map (EnvT e) . children . lower $ a
        where e = ask a 
    descendants a = replusify . map (EnvT e) . descendants . lower $ a
        where e = ask a 

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

    -- | Logs a message and returns the second argument unchanged.
    logmsgK::T.Text -> a -> l a
    logmsgK msg = (<$ logmsg msg) 

data LogEntry = TextEntry T.Text 
                |ImageEntry Image

instance Monad m => Loggy (Producer LogEntry m) where
    logentry = yield

instance (Monad l, Loggy l) => Loggy (LogicT l) where
    logentry = lift . logentry

instance (Monad l, Loggy l) => Loggy (MaybeT l) where
    logentry = lift . logentry

