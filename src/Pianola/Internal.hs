{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pianola.Internal (
        Nullipotent(..),
        Tag,
        Sealed(..),
        addTag
    ) where

import Prelude hiding (catch,(.),id)
import Data.Foldable (Foldable)
import Data.Traversable
import qualified Data.Text as T

newtype Nullipotent m a = Nullipotent { runNullipotent :: m a }
   deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Monad)
   
type Tag = T.Text

data Sealed m = Sealed {
       tags :: [Tag],
       unseal :: m ()
   }

addTag :: Tag -> Sealed m -> Sealed m
addTag t (Sealed ts a) = Sealed (t:ts) a

