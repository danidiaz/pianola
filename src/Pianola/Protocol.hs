{-# LANGUAGE FlexibleInstances #-}

module Pianola.Protocol (
        ServerError (..),
        ProtocolF,
        Protocol (..),
        call
    ) where

import Prelude hiding (catch,(.),id)
import Data.MessagePack
import Data.Attoparsec.ByteString
import qualified Data.Iteratee as I
import qualified Data.Text as T
import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Identity
import Data.Functor.Compose
import Control.Category
import Control.Applicative
import Control.Error
import Control.Monad.Trans
import Control.Monad.Free

type ProtocolF = Compose ((,) [BL.ByteString]) (I.Iteratee B.ByteString Identity) 

type Protocol = EitherT ServerError (Free ProtocolF)

call :: [BL.ByteString] -> (I.Iteratee B.ByteString Identity x) -> Protocol x
call bs i = lift . liftF $ Compose (bs,i)

data ServerError = SnapshotError Int Int
                 | ServerError T.Text
                 deriving Show

instance Unpackable (ServerError) where
    get = do
        tag <- get::Parser Int
        case tag of
            1 -> SnapshotError <$> get <*> get 
            2 -> ServerError <$> get


