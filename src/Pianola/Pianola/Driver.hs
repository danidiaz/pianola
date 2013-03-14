{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes #-}

module Pianola.Pianola.Driver (
    simpleDriver,
    DriverError,
    filePathStream,
    screenshotStream
) where 

import Prelude hiding (catch,(.),id,head,repeat,tail,map,iterate)
import Data.Stream.Infinite
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Category
import Control.Error
import Control.Proxy
import Control.Exception
import Control.Monad.State.Class
import Control.Monad.Logic
import Control.Concurrent (threadDelay)
import Control.Monad.RWS.Strict
import Pianola.Pianola
import Pianola.Util
import Pianola.Protocol
import Pianola.Protocol.IO

import System.FilePath

delayer :: MonadIO m => () -> Consumer ProxyFast Delay m a
delayer () = forever $ request () >>= liftIO . threadDelay . (*1000000)

logger:: MonadIO m => (forall b. IOException -> m b) -> m FilePath -> () -> Consu LogEntry m a
logger errHandler filegen () = forever $ do 
      entry <- request ()
      case entry of  
          TextEntry txt -> lift . convertErr . liftIO . try $ TIO.putStrLn txt
          ImageEntry image -> do
               file <- lift filegen
               lift . convertErr . liftIO . try $ B.writeFile file image
   where convertErr x = x >>= either errHandler return 

filePathStream :: String -> Int -> String -> FilePath -> Stream FilePath
filePathStream extension padding prefix folder = 
     let pad i c str = replicate (max 0 (i - length str)) c ++ str
         pathFromNumber =  combine folder 
                         . (\s -> prefix ++ s ++ extSeparator:extension) 
                         . pad padding '0' 
                         . show 
     in map pathFromNumber $ iterate succ 1 

screenshotStream :: FilePath -> Stream FilePath
screenshotStream = filePathStream  "png" 3 "pianola-capture-" 

data DriverError =
     DriverIOError IOException
    |PianolaIOError IOException 
    |PianolaParseError T.Text
    |PianolaSnapshotError
    |PianolaServerError
    |PianolaFailure
    deriving Show

simpleDriver :: Protocol o -> Endpoint -> Pianola Protocol LogEntry o a -> Stream FilePath -> EitherT DriverError IO a
simpleDriver snapshot endpoint pianola namestream = do
    let played = play snapshot pianola
        -- the lift makes a hole for an (EitherT DriverIOError...)
        rebased = hoist (hoist (hoist $ lift . runProtocol id)) $ played
        logprod = runMaybeT $ runProxy $ const rebased >-> delayer

        filegen = state $ \stream -> (head stream, tail stream) 

        logless = runProxy $ const logprod >-> logger left filegen

        errpeeled = runEitherT . runEitherT . runEitherT $ logless
    (result,_,())  <- lift $ runRWST errpeeled endpoint namestream
    case result of 
        Left e -> left $ case e of 
                   CommError ioerr -> PianolaIOError ioerr
                   ParseError perr -> PianolaParseError perr
        Right s -> case s of
            Left e -> left $ case e of 
                   ObsoleteRef -> PianolaSnapshotError 
                   InternalError -> PianolaServerError  
            Right r2 -> case r2 of 
                Left e -> left $ DriverIOError e
                Right r3 -> case r3 of
                    Nothing -> left PianolaFailure
                    Just a  -> return a

