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
import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Identity
import Data.Functor.Compose
import Control.Category
import Control.Error
import Control.Monad.Trans
import Control.Monad.Free

type ProtocolF = Compose ((,) [BL.ByteString]) (I.Iteratee B.ByteString Identity) 

type Protocol = EitherT ServerError (Free ProtocolF)

call :: [BL.ByteString] -> (I.Iteratee B.ByteString Identity x) -> Protocol x
call bs i = lift . liftF $ Compose (bs,i)

data ServerError = ObsoleteRef 
                 | InternalError 
                 deriving Show

instance Unpackable (ServerError) where
    get = do
        tag <- get::Parser Int
        case tag of
            1 -> do
                return $ ObsoleteRef
            2 -> do 
                return $ InternalError


