{-# LANGUAGE FlexibleInstances #-}

module Xanela.Types.Protocol.IO (
        RunInIOError(..),
        Endpoint(..),
        runFree,
        runProtocol
    ) where

import Prelude hiding (catch,(.))
import System.IO
import qualified Data.Text as T
import qualified Data.Iteratee as I
import qualified Data.Iteratee.IO.Handle as IH
import Network 
import Control.Category
import Control.Error
import Control.Applicative
import Control.Exception
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Logic
import Control.Monad.Free
import Data.Functor.Identity
import Control.Monad.Trans.Class   
import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as BL

import Xanela.Types.Protocol

data RunInIOError = CommError T.Text | ParseError T.Text

data Endpoint = Endpoint {
        hostName::HostName,
        portID::PortID
    }

runFree:: Free ProtocolF a -> EitherT RunInIOError (ReaderT Endpoint IO) a  
runFree ( Free (Call b i) ) = do
    let
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
   endp <- lift ask
   nextFree <- liftIO $ rpcCall endp $ doStuff iterIO    
   runFree nextFree 

runProtocol :: Protocol a -> EitherT ServerError (EitherT RunInIOError (ReaderT Endpoint IO)) a  
runProtocol = EitherT . runFree . runEitherT
