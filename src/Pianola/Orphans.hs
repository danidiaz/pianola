{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Pianola.Orphans where

import Prelude hiding (catch)
import Data.Tree
import Data.Aeson
import Data.Functor.Identity
import Control.Applicative

-- This orphan instance is possibly a bad idea, but I need to derive ToJSON and
-- FromJSON instances, and I can't be bothered to declare the instances
-- manually.
instance ToJSON a => ToJSON (Tree a) where
    toJSON (Node a forest)  = object ["root" .= a,"branches" .= forest]

instance FromJSON a => FromJSON (Tree a) where
    parseJSON (Object v) = Node <$> v .: "root" <*> v .: "branches"
    parseJSON _        = fail ""
--
-- Same for Identity
instance ToJSON a => ToJSON (Identity a) where
    toJSON (Identity a)  = object ["identity" .= a]

instance FromJSON a => FromJSON (Identity a) where
    parseJSON (Object v) = Identity <$> v .: "identity"
    parseJSON _        = fail ""
--

