{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Pianola.Util (
        throwIfZero,
        replusify,
        fromFold,
        decorate,
        the,
        prune,
        Treeish(..),
        Loggy(..),
        LogEntry(..),
        Image,
        jsonToText,
        logJSON
    ) where

import Prelude hiding (catch,(.),id)
import Control.Category
import Data.Functor.Identity
import Data.Tree
import Data.Foldable (toList)
import Data.MessagePack
import Data.Attoparsec.ByteString
import Control.Lens hiding ( (.=) )
import Control.Arrow
import Control.Monad
import Control.Comonad
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env    
import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Logic
import Control.Monad.Error
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Pipes

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
-- logging
type Image = B.ByteString

class Functor l => Loggy l where
    logentry::LogEntry -> l ()

    logmsg:: T.Text -> l ()
    logmsg = logentry . TextEntry

    logimg :: T.Text -> Image -> l ()
    logimg caption image = logentry $ ImageEntry caption image

data LogEntry = TextEntry T.Text 
                |ImageEntry T.Text Image 

instance Monad m => Loggy (Producer LogEntry m) where
    logentry = yield

instance (Monad l, Loggy l) => Loggy (LogicT l) where
    logentry = lift . logentry

instance (Monad l, Loggy l) => Loggy (MaybeT l) where
    logentry = lift . logentry

-----------------------------------------------------------------------
--
jsonToText :: ToJSON c => c -> T.Text
jsonToText = E.decodeUtf8 . BL.toStrict . encode . toJSON   

logJSON :: (Monad m,Loggy m,ToJSON c) => Kleisli m c ()
logJSON = Kleisli $ logmsg . jsonToText

-----------------------------------------------------------------------
-- orphan instances
--

-- This orphan instance is possibly a bad idea, but I need to derive ToJSON and
-- FromJSON instances, and I can't be bothered to declare the instances
-- manually.
instance ToJSON a => ToJSON (Tree a) where
    toJSON (Node a forest)  = object ["root" .= a,"branches" .= forest]

instance FromJSON a => FromJSON (Tree a) where
    parseJSON (Object v) = Node <$> v .: "root" <*> v .: "branches"
    parseJSON _        = fail ""

-- Same for Identity
instance ToJSON a => ToJSON (Identity a) where
    toJSON (Identity a)  = object ["identity" .= a]

instance FromJSON a => FromJSON (Identity a) where
    parseJSON (Object v) = Identity <$> v .: "identity"
    parseJSON _        = fail ""

-- Useful msgpack instances
instance (Unpackable a, Unpackable b) => Unpackable (Either a b) where
    get = do
        tag <- get::Parser Int
        case tag of
            1 -> Left <$> get
            0 -> Right <$> get

instance Unpackable a => Unpackable (Tree a) where
    get = Node <$> get <*> get

instance Unpackable a => Unpackable (Identity a) where
    get = Identity <$> get

-----------------------------------------------------------------------



