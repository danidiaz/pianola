-- | This module should not be imported by clients unless for the purpose of
-- extending the library. 
--
-- The constructors of the data types defined in this module are meant to be
-- hidden from the client. 

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pianola.Internal (
        Query(..),
        Tag,
        Change(..),
        addTag
    ) where

import Prelude hiding (catch,(.),id)
import Data.Foldable (Foldable)
import Data.Traversable
import qualified Data.Text as T

-- | Wraps a monad in order to tag those operations which don't actually change
-- the state of the remote system. For example: taking a screenshot doesn't
-- change the state of a GUI, as opposed to clicking a button.
newtype Query m a = Query { runQuery :: m a }
   deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Monad)
   
type Tag = T.Text

-- | Encapsulates a monadic action so that clients can't manipulate it in any
-- way, only dispatch it to some function.
--
-- There may be tags attached that describe the action. Clients should be able
-- to inspect the tags.
data Change m = Change {
       tags :: [Tag],
       unseal :: m ()
   }

addTag :: Tag -> Change m -> Change m
addTag t (Change ts a) = Change (t:ts) a

