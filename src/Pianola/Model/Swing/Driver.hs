{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Pianola.Model.Swing.Driver (
    simpleSwingDriver
) where 

import Prelude hiding (catch,(.),id,head,repeat,tail,map,iterate)
import System.IO
import System.Environment
import System.Console.GetOpt
import Data.Tree
import Data.Stream.Infinite
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network 
import Control.Category
import Control.Error
import Control.Monad.Trans.Either
import Control.Applicative
import Control.Proxy
import Control.MFunctor
import Control.Monad
import Control.Comonad
import Control.Exception
import Control.Monad.State.Class
import Control.Monad.Free
import Control.Monad.Trans.Reader
import Control.Monad.Logic
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Free  
import Control.Concurrent (threadDelay)
import Control.Monad.RWS.Class
import Control.Monad.RWS.Strict
import Pianola.Pianola
import Pianola.Geometry
import Pianola.Util
import Pianola.Protocol
import Pianola.Model.Swing
import Pianola.Pianola.Driver
import Pianola.Protocol.IO
import Pianola.Model.Swing.Protocol (snapshot)

import System.FilePath

simpleSwingDriver :: Endpoint -> Pianola Protocol LogEntry (GUI Protocol) a -> Stream FilePath -> EitherT DriverError IO a
simpleSwingDriver = simpleDriver snapshot
