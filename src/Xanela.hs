{-# LANGUAGE TemplateHaskell,GeneralizedNewtypeDeriving #-}

module Xanela (
        Xanela,
        unXanela, 
        Endpoint (..),
        gui,
        Window,
        WindowInfo (..),
        Component,
        ComponentInfo (..),
        ComponentType (..)
    ) where

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
import Data.Foldable
import Data.Traversable
import qualified Data.Text as T
import qualified Data.Iteratee as I
import qualified Data.Iteratee.IO.Handle as IH
import qualified Data.Attoparsec.Iteratee as AI
import qualified Data.ByteString.Lazy as BL
import Control.Category
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Applicative
import Control.Exception
import Network
import Blaze.ByteString.Builder
import Data.MessagePack
import Data.MessagePack.Object
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Debug.Trace (trace)

newtype Xanela a = Xanela { unXanela:: ReaderT Endpoint IO a }
  deriving (Functor, Monad, MonadIO, Applicative)

data Endpoint = Endpoint {
        hostName::HostName,
        portID::PortID
    }

instance Show (Xanela a) where
    show x = "_x_"

gui:: Xanela [Window]
gui = Xanela $ do
  endpoint <- ask
  liftIO $ rpcCall endpoint $ \h -> do
      BL.hPutStr h . pack $ "get"
      hFlush h
      I.run =<< IH.enumHandle 1024 h (AI.parserToIteratee get)

type ComponentID = Int

click:: ComponentID -> Xanela ()
click cid = Xanela $ do
  endpoint <- ask
  liftIO $ rpcCall endpoint $ \h -> do
      BL.hPutStr h . pack $ "click"
      BL.hPutStr h . pack $ cid
      hFlush h
      -- I.run =<< IH.enumHandle 1024 h (AI.parserToIteratee get)

rpcCall :: Endpoint -> (Handle -> IO a) -> IO a
rpcCall endpoint what2do = withSocketsDo $ do
  bracket (connectTo (hostName endpoint) (portID endpoint))
          hClose
          what2do

type Window = Tree WindowInfo

data WindowInfo = WindowInfo
    {
        _windowTitle::T.Text,
        _windowDim::(Int,Int),
        _topc::Component
    } deriving Show            

instance Unpackable WindowInfo where
    get = do
        v1 <- get
        v2 <- get
        v3 <- get
        return (WindowInfo v1 v2 v3)

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

data ComponentType =
     Panel
    |Button (Maybe Bool) (Xanela ())
    |TextField T.Text
    |Other T.Text
    deriving Show

instance Unpackable ComponentType where
    get = do
        typeTag <- get
        case typeTag::Int of
            1 -> return Panel
            2 -> do 
                v2 <- get
                v3 <- get
                return $ Button v3 (click v2)
            3 -> do
                v2 <- get
                return (TextField v2)
            4 -> do
                v2 <- get
                return (Other v2)

instance Unpackable a => Unpackable (Tree a) where
    get = do
        v1 <- get
        v2 <- get
        return (Node v1 v2)

