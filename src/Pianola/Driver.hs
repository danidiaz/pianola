{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes #-}

module Pianola.Driver (
    drive,
    DriverError(..),
    filePathStream,
    screenshotStream,
    module Pianola,
    module Pianola.Protocol,
    module Pianola.Protocol.IO
) where 

import Prelude hiding (catch,(.),id,head,repeat,tail,map,iterate)
--import Data.Stream.Infinite
import Data.Functor.Identity
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Category
import Control.Comonad
import Control.Exception
import Control.Monad.State.Class
import Control.Monad.Logic
import Control.Comonad.Cofree
import Control.Concurrent (threadDelay)
import Control.Monad.RWS.Strict
import Pianola
import Pianola.Util
import Pianola.Protocol
import Pianola.Protocol.IO
import Pipes
import Control.Monad.Trans.Maybe
import Control.Monad.Error

import System.FilePath

delayer :: MonadIO m => Consumer Delay m a
delayer = forever $ await >>= liftIO . threadDelay . (*1000000)

logger:: MonadIO m => (forall b. IOException -> m b) -> m FilePath -> Consumer LogEntry m a
logger errHandler filegen = forever $ do 
      entry <- await
      case entry of  
          TextEntry txt -> lift . convertErr . liftIO . try $ TIO.putStrLn txt
          ImageEntry image -> do
               file <- lift filegen
               lift . convertErr . liftIO . try $ B.writeFile file image
   where convertErr x = x >>= either errHandler return 

-- | A more general version of 'screenshotStream', which allows the client to
-- specify the prefix before the file number, the amount of padding for the
-- file number, and the suffix after the file number.
filePathStream :: String -> Int -> String -> FilePath -> Cofree Identity FilePath
filePathStream extension padding prefix folder = 
     let pad i c str = replicate (max 0 (i - length str)) c ++ str
         pathFromNumber =  combine folder 
                         . (\s -> prefix ++ s ++ extSeparator:extension) 
                         . pad padding '0' 
                         . show 
     in unfold (\n -> (pathFromNumber n, Identity $ succ n)) 1 

-- | Returns an infinite stream of filenames for storing screenshots, located
-- in the directory supplied as a parameter.
screenshotStream :: FilePath -> Cofree Identity FilePath
screenshotStream = filePathStream  "png" 3 "pianola-capture-" 

-- | Possible failure outcomes when running a pianola computation.
data DriverError =
    -- | Local exception while storing screenshots or log messages.  
     DriverIOError IOException
    -- | Exception when connecting the remote system.
    |PianolaIOError IOException 
    -- | Remote system returns unparseable data. 
    |PianolaParseError T.Text
    -- | An operation was requested on an obsolete snapshot (first integer) of
    -- the remote system (whose current snapshot number is the second integer).
    |PianolaSnapshotError Int Int
    -- | Server couldn't complete requested operation (either because it
    -- doesn't support the operation or because of an internal error.)
    |PianolaServerError T.Text
    -- | Failure from a call to 'pfail' or from a 'Glance' without results. 
    |PianolaFailure String
    deriving Show

-- Spurious instance of error
instance Error DriverError where
    noMsg = PianolaFailure ""

-- | Runs a pianola computation. Receives as argument a monadic action to
-- obtain snapshots of type /o/ of a remote system, a connection endpoint to
-- the remote system, a 'Pianola' computation with context of type /o/ and
-- return value of type /a/, and an infinite stream of filenames to store the
-- screenshots. Textual log messages are written to standard output. The
-- computation may fail with an error of type 'DriverError'. 
--
-- See also 'Pianola.Model.Swing.Driver.simpleSwingDriver'.
drive :: Protocol o -> Endpoint -> Pianola Protocol LogEntry o a -> Cofree Identity FilePath -> ErrorT DriverError IO a
drive snapshot endpoint pianola namestream = do
    let played = play snapshot pianola
        -- the lift makes a hole for an (EitherT DriverIOError...)
        rebased = hoist (hoist (hoist $ lift . runProtocol id)) $ played
        logprod = runErrorT $ runEffect $ rebased >-> delayer

        filegen = state $ \stream -> (extract stream, runIdentity . unwrap $ stream) 

        logless = runEffect $ logprod >-> logger throwError filegen

        errpeeled = runErrorT . runErrorT . runErrorT $ logless
    (result,_,())  <- lift $ runRWST errpeeled endpoint namestream
    case result of 
        Left e -> throwError $ case e of 
                   CommError ioerr -> PianolaIOError ioerr
                   ParseError perr -> PianolaParseError perr
        Right s -> case s of
            Left e -> throwError $ case e of 
                   SnapshotError u v -> PianolaSnapshotError u v
                   ServerError txt -> PianolaServerError txt
            Right r2 -> case r2 of 
                Left e -> throwError $ DriverIOError e
                Right r3 -> case r3 of
                    Left msg -> throwError $ PianolaFailure msg
                    Right a  -> return a

