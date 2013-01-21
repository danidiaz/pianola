{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Pianola.Protocol (
        ServerError (..),
        ProtocolF (..),
        Protocol (..),
        getgui
    ) where

import Prelude hiding (catch,(.),id)
import Data.MessagePack
import Data.Attoparsec.ByteString
import qualified Data.Text as T
import qualified Data.Iteratee as I
import qualified Data.Attoparsec.Iteratee as AI
import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Identity
import Data.Functor.Compose
import Control.Category
import Control.Error
import Control.Monad
import Control.Applicative
import Control.Monad.Base
import Control.Monad.Trans.Class
import Control.Monad.Free

import Pianola.Types
import Pianola.Util

type ProtocolF x = Compose ((,) [BL.ByteString]) (I.Iteratee B.ByteString Identity) x

type Protocol = EitherT ServerError (Free ProtocolF)

call :: [BL.ByteString] -> (I.Iteratee B.ByteString Identity x) -> Protocol x
call bs i = lift . liftF $ Compose (bs,i)

data ServerError = ObsoleteRef | InternalError

instance Unpackable (ServerError) where
    get = do
        tag <- get::Parser Int
        case tag of
            1 -> do
                return $ ObsoleteRef
            2 -> do 
                return $ InternalError


