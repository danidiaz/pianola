{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Pianola.Orphans where

import Prelude hiding (catch)
import Data.Tree
import Data.Aeson
import Data.Functor.Identity
import Control.Applicative
import Data.MessagePack
import Data.Attoparsec.ByteString

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

