{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Pianola.Main (
        makeMain
    ) where

import Prelude hiding (catch,(.),id)
import Control.Category
import Data.Functor.Identity
import Data.Tree
import Data.Foldable (toList)
import Data.MessagePack
import Data.Attoparsec.ByteString
import Control.Lens
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

import Pianola.Internal

data Opts = Opts
    { host :: Integer
    , port :: String
    , testName :: String
    , listTests :: Bool
    }

makeMain :: IO ()
makeMain = undefined



