{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pianola.Internal (
        Nullipotent(..),
        Tag,
        Sealed(..),
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

newtype Nullipotent m a = Nullipotent { runNullipotent :: m a }
   deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Monad)
   
--instance (Monad m) => Monad (Nullipotent m) where
--   (Nullipotent m) >>= k = Nullipotent $ m >>= (runNullipotent . k)
--   return = Nullipotent . return
   
type Tag = T.Text

data Sealed m = Sealed {
       tags :: [Tag],
       unseal :: m ()
   }

addTag :: Tag -> Sealed m -> Sealed m
addTag t (Sealed ts a) = Sealed (t:ts) a

