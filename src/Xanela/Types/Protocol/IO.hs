{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

module Xanela.Types.Protocol.IO (
        RunInIOError(..),
        Endpoint(..),
        runInIO
    ) where

import Prelude hiding (catch,(.))
import System.IO
import System.Environment
import System.Console.GetOpt
import Data.Char
import qualified Data.Map as M
import Data.List
import Data.Default
import Data.Tree
--import Data.Foldable
import Data.Traversable
import qualified Data.Text as T
import qualified Data.Iteratee as I
import qualified Data.Iteratee.IO.Handle as IH
import qualified Data.Attoparsec.Iteratee as AI
import Data.Attoparsec.ByteString
-- import qualified Data.ByteString as BL hiding (pack)
import Control.Category
import Control.Error
import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Control.Exception
import Network 
-- import Blaze.ByteString.Builder
import Data.MessagePack
import Data.MessagePack.Object
import Control.Concurrent
import Control.Monad
import Control.Monad.Logic
import Control.Monad.Free
import Control.Monad.Trans
import Control.Monad.Identity
import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as BL
import Xanela.Types.Protocol
import Debug.Trace (trace)

data RunInIOError = CommError T.Text | ParseError T.Text

data Endpoint = Endpoint {
        hostName::HostName,
        portID::PortID
    }

runInIO:: Free ProtocolF a -> EitherT RunInIOError (ReaderT Endpoint IO) a  
runInIO ( Free x ) = case x of 
    Call b i -> let
                    iterIO = I.ilift (return . runIdentity) i

                    rpcCall :: Endpoint -> (Handle -> IO b) -> IO b
                    rpcCall endpoint what2do = withSocketsDo $ do
                          bracket (connectTo (hostName endpoint) (portID endpoint))
                                  hClose
                                  what2do
                    
                    doStuff ii h = do
                        mapM_ (BL.hPutStr h) b
                        hFlush h
                        I.run =<< IH.enumHandle 1024 h ii   
                in do
                       endp <- lift ask
                       nextFree <- liftIO $ rpcCall endp $ doStuff iterIO    
                       runInIO nextFree 
    Delay i n -> do
                    liftIO $ threadDelay i
                    runInIO n

