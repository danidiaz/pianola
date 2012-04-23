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
import qualified Data.Text as T
import Control.Category
import Control.Monad
import Control.Monad.Error
import Control.Applicative
import Control.Exception

import Network.MessagePackRpc.Client
 
import Control.Concurrent
import Control.Monad
 
hello :: RpcMethod (T.Text -> Int -> IO T.Text)
hello = method "hello"
 
main :: IO ()
main = do
  args <- getArgs 
  conn <- connect (head args) 26060
  (print .T.unpack)  =<< hello conn (T.pack "hello") 4

