{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (catch,(.))
import System.IO
import System.Environment
import System.Console.GetOpt
import Data.Char
import qualified Data.Map as M
import Data.List
import Data.Lens.Common
import Data.Lens.Template
import Data.Default
import Data.Tree
import qualified Data.Text as T
import qualified Data.Iteratee as I
import qualified Data.Iteratee.IO.Handle as IH
import qualified Data.Attoparsec.Iteratee as AI
import qualified Data.ByteString.Lazy as BL
import Control.Category
import Control.Monad
import Control.Monad.Error
import Control.Applicative
import Control.Exception
import Network
import Blaze.ByteString.Builder
import Data.MessagePack
import Data.MessagePack.Object
import Control.Concurrent
import Control.Monad
import Xanela
import Debug.Trace (trace)
 
main :: IO ()
main = do
  args <- getArgs 
  let addr = head args
      port = PortNumber $ fromIntegral 26060
  wlist <- rpcCall addr port getGuiState
  mapM_ (putStrLn . show) wlist
   
