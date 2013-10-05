{-# LANGUAGE ScopedTypeVariables #-}

module Pianola.Protocol.IO (
        RunInIOError(..),
        Endpoint(..),
        runProtocol
    ) where

import Prelude hiding (catch,(.))
import System.IO
import qualified Data.Text as T
import qualified Data.Iteratee as I
import qualified Data.Iteratee.IO.Handle as IH
import qualified Data.Attoparsec.Iteratee as AI 
import Network 
import Data.Functor.Compose
import Control.Category
import Control.Error
import Control.Exception
import Control.Monad.Logic
import Control.Monad.Free
import Data.Functor.Identity
import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Reader
import Pianola.Protocol
import Data.MessagePack

iterget :: (Monad m, Unpackable a) => I.Iteratee B.ByteString m a 
iterget = AI.parserToIteratee get

data RunInIOError = CommError IOException 
                  | ParseError T.Text
                  deriving Show

data Endpoint = Endpoint {
        hostName::HostName,
        portID::PortID
    }

runFree:: (MonadIO m, MonadReader r m) => (r -> Endpoint) -> Free ProtocolF a -> EitherT RunInIOError m a  
runFree lens ( Free (Compose (b,i)) ) = do
    --let iterIO = I.ilift (return . runIdentity) i
    let iterIO = AI.parserToIteratee i

        rpcCall :: Endpoint -> (Handle -> IO b) -> IO b
        rpcCall endpoint what2do = withSocketsDo $ do
              bracket (connectTo (hostName endpoint) (portID endpoint))
                      hClose
                      what2do
        
        doStuff ii h = do
            mapM_ (BL.hPutStr h) b
            hFlush h
            I.run =<< IH.enumHandle 1024 h ii   
    endp <- lift $ asks lens
    let ioErrHandler = \(ex :: IOException) -> return . Left . CommError $ ex
        parseErrHandler = \(ex :: AI.ParseError) -> return . Left . ParseError . T.pack . show $ ex   
    nextFree <- EitherT . liftIO $ 
            catches (fmap Right $ rpcCall endp $ doStuff iterIO) 
            [Handler ioErrHandler, Handler parseErrHandler]
    runFree lens nextFree 
runFree _ ( Pure a ) = return a 

-- | Runs a sequence of RPC calls in a base monad which has access to an
-- 'Endpoint' value which identifies the server. An accessor function must be
-- provided to extract the Endpoint from the base monad's environment, which
-- may be more general. 
runProtocol :: (MonadIO m, MonadReader r m) => (r -> Endpoint) -> Protocol a -> EitherT ServerError (EitherT RunInIOError m) a  
runProtocol lens = EitherT . runFree lens . runEitherT
