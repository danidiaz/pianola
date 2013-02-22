{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Pianola.Pianola.Driver (
    simpleDriver,
    DriverError,
    filePathStream,
    screenshotStream
) where 

import Prelude hiding (catch,(.),id,head,repeat,tail,map,iterate)
import System.IO
import System.Environment
import System.Console.GetOpt
import Data.Tree
import Data.Stream.Infinite
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network 
import Control.Category
import Control.Error
import Control.Monad.Trans.Either
import Control.Applicative
import Control.Proxy
import Control.MFunctor
import Control.Monad
import Control.Comonad
import Control.Exception
import Control.Monad.State.Class
import Control.Monad.Free
import Control.Monad.Trans.Reader
import Control.Monad.Logic
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Free  
import Control.Concurrent (threadDelay)
import Control.Monad.RWS.Class
import Control.Monad.RWS.Strict
import Pianola.Pianola
import Pianola.Geometry
import Pianola.Util
import Pianola.Protocol
import Pianola.Model.Swing
import Pianola.Model.Swing.Protocol (snapshot)
import Pianola.Protocol.IO

import System.FilePath

delayer :: MonadIO m => () -> Consumer ProxyFast Delay m a
delayer () = forever $ request () >>= liftIO . threadDelay . (*1000000)

logger:: MonadIO m => (IOException -> m a) -> m FilePath -> () -> LogConsumer m a
logger errHandler filegen () = forever $ do 
      entry <- request ()
      case entry of  
          TextEntry txt -> liftIO $ TIO.putStrLn txt
          ImageEntry image -> do
               file <- lift filegen
               liftIO $ B.writeFile file image

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
     DriverIOError
    |PianolaIOError
    |ProtocolError
    |PianolaFailure

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
        Left _ -> left DriverIOError
        Right s -> case s of
            Left _ -> left PianolaIOError
            Right r2 -> case r2 of 
                Left _ -> left ProtocolError
                Right r3 -> case r3 of
                    Nothing -> left PianolaFailure
                    Just a  -> return a
