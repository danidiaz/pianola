{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Pianola.Util (
        throwIfZero,
        replusify,
--        tomaybet,
        fromFold,
        decorate,
        the,
        prune,
        Treeish(..),
        Loggy(..),
        LogEntry(..),
        Image,
        Query(runQuery),
        Tag,
        Change(tags,unseal),
        addTag
    ) where

import Prelude hiding (catch,(.),id)
import Control.Category
import Data.Functor.Identity
import Data.Tree
import Data.Foldable (toList)
import Data.MessagePack
import Data.Attoparsec.ByteString
import Control.Lens
import Control.Arrow
import Control.Monad
import Control.Comonad
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env    
import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Logic
import Control.Monad.Error
import qualified Data.Text as T
import qualified Data.ByteString as B
import Pipes

import Pianola.Internal

--
throwIfZero :: MonadError e m => e -> m (Maybe a) -> m a 
throwIfZero e m = m >>= maybe (throwError e) return

-- | Convenience function to transform a list into any 'MonadPlus'.
replusify:: (Foldable f, MonadPlus m) => f a -> m a
replusify = msum . map return . toList

-- | Class of types whose values have descendants1 of the same type as themselves.
class Treeish l where
    -- | All direct or indirect descendants, plus the original value.
    descendants :: MonadPlus m => Kleisli m l l
    -- | Direct descendants.
    descendants1 :: MonadPlus m => Kleisli m l l
     
    descendantsN :: MonadPlus m => Int -> Kleisli m l l
    descendantsN depth =  foldr (>>>) returnA (replicate depth $ descendants1)

instance Treeish (Tree a) where
    descendants1 = Kleisli $ replusify . subForest
    descendants = Kleisli $ replusify . flatten . duplicate

instance (Comonad c, Treeish (c a)) => Treeish (EnvT e c a) where
    descendants1 = Kleisli $ \a -> let e = ask a 
                                   in replusify . map (EnvT e) . (runKleisli descendants1) . lower $ a
    descendants = Kleisli $ \a -> let e = ask a 
                                  in replusify . map (EnvT e) . (runKleisli descendants) . lower $ a

-----------------------------------------------------------------------

fromFold :: MonadPlus m => Fold a b -> Kleisli m a b
fromFold f = Kleisli $ replusify . toListOf f

the :: Comonad c => Fold (c a) a
the = to extract

decorate ::  (Comonad c, MonadPlus m) => Fold a (c b) -> Kleisli m a (EnvT a c b)
decorate f = Kleisli $ \x -> replusify . map (EnvT x) $ toListOf f x

prune :: MonadPlus m => Fold a b -> (b -> Bool) -> Kleisli m a a
prune f p = Kleisli $ \x -> guard (anyOf f p x) >> return x 

-----------------------------------------------------------------------

-- useful msgpack instances
instance (Unpackable a, Unpackable b) => Unpackable (Either a b) where
    get = do
        tag <- get::Parser Int
        case tag of
            1 -> Left <$> get
            0 -> Right <$> get

instance Unpackable a => Unpackable (Identity a) where
    get = Identity <$> get

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

