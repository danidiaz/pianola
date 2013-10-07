{-# LANGUAGE ScopedTypeVariables #-}

module Pianola.Protocol.IO (
        RunInIOError(..),
        Endpoint(..),
        runProtocol
    ) where

import Prelude hiding (catch,(.))
import System.IO
import qualified Data.Text as T
import Network 
import Data.Functor.Compose
import Data.Bifunctor
import Control.Category
import Control.Exception
import Control.Monad.Logic
import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Functor.Identity
import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as BL
import Pianola.Protocol
import Data.MessagePack
import Pipes.ByteString
import Pipes.Attoparsec
import Control.Monad.Error

data RunInIOError = CommError IOException 
                  | ParseError T.Text
                  deriving Show

-- Spurious error instance. would putting and error here be too awful?
instance Error RunInIOError where
    noMsg = ParseError T.empty

data Endpoint = Endpoint {
        hostName::HostName,
        portID::PortID
    }

runFree:: (MonadIO m, MonadReader r m) => (r -> Endpoint) -> Free ProtocolF a -> ErrorT RunInIOError m a  
runFree lens ( Free (Compose (b,parser)) ) = do
    --let iterIO = I.ilift (return . runIdentity) i
    endp <- lift $ asks lens
    let rpcCall :: Endpoint -> (Handle -> IO b) -> IO b
        rpcCall endpoint what2do = withSocketsDo $ do
              bracket (connectTo (hostName endpoint) (portID endpoint))
                      hClose
                      what2do
        
        doStuff h = do
            mapM_ (BL.hPutStr h) b
            hFlush h
            evalStateT (parse parser) (fromHandle h)

        ioErrHandler = \(ex :: IOException) -> return . Left . CommError $ ex
        parseErrHandler = ParseError . T.pack . show
    nextFree <- ErrorT . liftIO $ 
            catches (fmap (bimap parseErrHandler snd) $ rpcCall endp $ doStuff) 
            [Handler ioErrHandler]
    runFree lens nextFree 
runFree _ ( Pure a ) = return a 

-- | Runs a sequence of RPC calls in a base monad which has access to an
-- 'Endpoint' value which identifies the server. An accessor function must be
-- provided to extract the Endpoint from the base monad's environment, which
-- may be more general. 
runProtocol :: (MonadIO m, MonadReader r m) => (r -> Endpoint) -> Protocol a -> ErrorT ServerError (ErrorT RunInIOError m) a  
runProtocol lens = ErrorT . runFree lens . runErrorT
