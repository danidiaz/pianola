{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Pianola.Util (
        replusify,
        tomaybet,
        perhaps,
        extract',
        fromFold,
        the,
        the',
        forWhich,
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
import Data.Foldable (toList)
import Data.MessagePack
import Data.Attoparsec.ByteString
import Control.Lens
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
replusify:: (Foldable f, MonadPlus m) => f a -> m a
replusify = msum . map return . toList

-- | Transforms a zero-or-many result into a zero-or-one result.
tomaybet:: Monad m => LogicT m a -> MaybeT m a
tomaybet = MaybeT . liftM replusify . observeManyT 1

-- | Class of types whose values have children' of the same type as themselves.
class Treeish l where
    -- | Direct descendants.
    children' :: MonadPlus m => l -> m l
    -- | All direct or indirect descendants, plus the original value.
    descendants :: MonadPlus m => l -> m l

instance Treeish (Tree a) where
    children' = replusify . subForest
    descendants = replusify . flatten . duplicate

instance (Comonad c, Treeish (c a)) => Treeish (EnvT e c a) where
    children'  a = replusify . map (EnvT e) . children' . lower $ a
        where e = ask a 
    descendants a = replusify . map (EnvT e) . descendants . lower $ a
        where e = ask a 

-----------------------------------------------------------------------

fromFold :: MonadPlus m => Fold a b -> a -> m b
fromFold f = replusify . toListOf f

perhaps :: (Foldable f, MonadPlus m) => f a -> m a
perhaps = replusify

extract' :: Comonad c => IndexPreservingGetter (c a) a
extract' = to extract

the :: (Comonad c, MonadPlus m) => Fold a b -> c a -> m b 
the f = replusify . toListOf f . extract  

the' ::  (Comonad c, Comonad c', MonadPlus m) => Fold a (c' b) -> c a -> m (EnvT (c a) c' b)
the' f x = replusify . map (EnvT x) $ toListOf f (extract x)

forWhich :: (Comonad c, MonadPlus m) => Fold a b -> (b -> Bool) -> c a -> m (c a)
forWhich f p x = guard (anyOf (to extract . f) p $ x) >> return x 

--the' ::  (Comonad c, Comonad c', Monad m) => (a -> c' b) -> c a -> m (EnvT (c a) c' b)
--the' f x = return . EnvT x $ f (extract x)

--allThe' ::  (Comonad c, Comonad c', Foldable f, MonadPlus m) => (a -> f (c' b)) -> c a -> m (EnvT (c a) c' b)
--allThe' f x = replusify . map (EnvT x) . toList $ f (extract x)

-----------------------------------------------------------------------

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

