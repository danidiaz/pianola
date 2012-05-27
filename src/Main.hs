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
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Attoparsec as CA
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

--
--
--
data Connection
  = Connection
    { connHandle :: MVar Handle }

-- | Connect to RPC server
connect :: String -- ^ Host name
           -> Int -- ^ Port number
           -> IO Connection -- ^ Connection
connect addr port = withSocketsDo $ do
  h <- connectTo addr (PortNumber $ fromIntegral port)
  mh <- newMVar h
  return $ Connection
    { connHandle = mh
    }

-- | Disconnect a connection
disconnect :: Connection -> IO ()
disconnect Connection { connHandle = mh } =
  hClose =<< takeMVar mh

rpcGETCALL :: Connection -> IO [Window]
rpcGETCALL Connection{ connHandle = mh } = withMVar mh $ \h -> do
  BL.hPutStr h $ pack $ "get"
  hFlush h
  C.runResourceT $ CB.sourceHandle h C.$$ do
    wlist <- CA.sinkParser get
    --BL.hPutStr h $ pack "close"
    --hFlush h
    return wlist

rpcCLOSE :: Connection -> IO ()
rpcCLOSE Connection{ connHandle = mh } = withMVar mh $ \h -> do
  BL.hPutStr h $ pack $ "close"
  hFlush h
--
--
--
 
main :: IO ()
main = do
  args <- getArgs 
  conn <- connect (head args) 26060
  wlist <- rpcGETCALL conn
  mapM_ (putStrLn . show) wlist
  rpcCLOSE conn
  disconnect conn
   
