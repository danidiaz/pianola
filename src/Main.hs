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
import Control.Monad.Reader
import Xanela
import Xanela.Combinators
import Debug.Trace (trace)
 
main :: IO ()
main = do
  args <- getArgs 
  let addr = head args
      port = PortNumber . fromIntegral $ 26060
      endpoint = Endpoint addr port
  wlist <- runReaderT (unXanela gui) endpoint
  --mapM_ (putStrLn . show) wlist
  runReaderT (unXanela $ gui >>= clickButtonWithText) endpoint
  wlist2 <- runReaderT (unXanela $ gui) endpoint
  mapM_ (putStrLn . drawTree . fmap (show . _componentType) . _topc) (concatMap flatten wlist2)
      
   
