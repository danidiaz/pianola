module Pianola.Protocol.IO (
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
import Data.Functor.Compose
import Control.Category
import Control.Error
import Control.Exception
import Control.Monad.Logic
import Control.Monad.Free
import Data.Functor.Identity
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Reader
import Pianola.Protocol

data RunInIOError = CommError T.Text | ParseError T.Text

data Endpoint = Endpoint {
        hostName::HostName,
        portID::PortID
    }

runFree:: (MonadIO m, MonadReader r m) => (r -> Endpoint) -> Free ProtocolF a -> EitherT RunInIOError m a  
runFree lens ( Free (Compose (b,i)) ) = do
    let iterIO = I.ilift (return . runIdentity) i

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
    nextFree <- liftIO $ rpcCall endp $ doStuff iterIO    
    runFree lens nextFree 
runFree _ ( Pure a ) = return a 

runProtocol :: (MonadIO m, MonadReader r m) => (r -> Endpoint) -> Protocol a -> EitherT ServerError (EitherT RunInIOError m) a  
runProtocol lens = EitherT . runFree lens . runEitherT
