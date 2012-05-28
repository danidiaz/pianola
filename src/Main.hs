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
import Debug.Trace (trace)
 
type Window = Tree WindowInfo

data WindowInfo = WindowInfo
    {
        _windowTitle::T.Text,
        _windowDim::(Int,Int),
        _topc::Component
    } deriving Show            

type Component = Tree ComponentInfo

data ComponentInfo = ComponentInfo
    {
        _pos::(Int,Int),
        _dim::(Int,Int),
        _name::Maybe T.Text,
        _tooltip::Maybe T.Text,
        _text::Maybe T.Text,
        _enabled::Bool,
        _componentType::ComponentType
    } deriving Show

data ComponentType =
     Panel
    |Button T.Text
    |TextField T.Text
    |Other T.Text
    deriving Show

instance Unpackable a => Unpackable (Tree a) where
    get = do
        v1 <- get
        v2 <- get
        return (Node v1 v2)
    
instance Unpackable WindowInfo where
    get = do
        v1 <- get
        v2 <- get
        v3 <- get
        return (WindowInfo v1 v2 v3)

instance Unpackable ComponentInfo where
    get = do
        v1 <- get
        v2 <- get
        v3 <- get
        v4 <- get
        v5 <- get
        v6 <- get
        v7 <- get
        return (ComponentInfo v1 v2 v3 v4 v5 v6 v7)

instance Unpackable ComponentType where
    get = do
        typeTag <- get
        case typeTag::Int of
            1 -> return Panel
            2 -> do 
                v2 <- get
                return (Button v2)
            3 -> do
                v2 <- get
                return (TextField v2)
            4 -> do
                v2 <- get
                return (Other v2)

rpcCall :: HostName -> PortID -> (Handle -> IO a) -> IO a
rpcCall host port what2do = withSocketsDo $ do
  bracket (connectTo host port)
          hClose
          what2do
  
getGuiState :: Handle -> IO [Window]
getGuiState h = do   
  BL.hPutStr h . pack $ "get"
  hFlush h
  I.run =<< IH.enumHandle 1024 h (AI.parserToIteratee get)
    
main :: IO ()
main = do
  args <- getArgs 
  let addr = head args
      port = PortNumber $ fromIntegral 26060
  wlist <- rpcCall addr port getGuiState
  mapM_ (putStrLn . show) wlist
   
