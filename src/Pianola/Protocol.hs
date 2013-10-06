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
--import qualified Data.Iteratee as I
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

-- | A 'Functor' which models a RPC call as a pair in which the first component
-- is a list of bytestrings (the arguments of the call) and the second is a
-- pure 'Iteratee' that consumes the bytes sent from the server and returns the
-- response of the call.
type ProtocolF = Compose ((,) [BL.ByteString]) Parser

-- | A monad to represent interactions with a remote server. A free monad over
-- a RPC call functor, augmented with some error conditions.
type Protocol = EitherT ServerError (Free ProtocolF)

-- | Constructs a RPC call from a packed list of arguments and a pure
-- 'Iteratee' to consume the response.
call :: [BL.ByteString] -> Parser x -> Protocol x
call bs i = lift . liftF $ Compose (bs,i)

data ServerError = 
                   -- | Client targeted obsolete snapshot. 
                   SnapshotError Int Int
                   -- | Server couldn't perform the requested operation.
                 | ServerError T.Text
                 deriving Show

instance Unpackable (ServerError) where
    get = do
        tag <- get::Parser Int
        case tag of
            1 -> SnapshotError <$> get <*> get 
            2 -> ServerError <$> get


