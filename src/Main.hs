{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Prelude hiding (catch,(.))
import System.IO
import System.Environment
import System.Console.GetOpt
import Data.Tree
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network 
import Control.Category
import Control.Error
import Control.Applicative
import Control.Proxy
import Control.Monad
import Control.Exception
import Control.Monad.Base
import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.Logic
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Free  

import Xanela.Util
import Xanela.Types
import Xanela.Types.Combinators
import Xanela.Types.Protocol
import Xanela.Types.Protocol.IO

class XanelaLog l where
    xanelalog::LogEntry -> l ()
    logmsg::T.Text -> l ()
    logimg::Image -> l ()

    logmsg = xanelalog . TextEntry
    logimg = xanelalog . ImageEntry

data LogEntry = TextEntry T.Text 
                |ImageEntry Image

type LogProducer m = Server () LogEntry m
type LogConsumer m = Client () LogEntry m


mapProxy f = Proxy . f . unProxy 

instance Monad m => XanelaLog (LogProducer m) where
    xanelalog = respond 

instance (Monad l, XanelaLog l) => XanelaLog (LogicT l) where
    xanelalog = lift . xanelalog

instance (Monad l, XanelaLog l) => XanelaLog (MaybeT l) where
    xanelalog = lift . xanelalog

testCase:: (Monad m, MonadBase n m) => GUI n -> MaybeT (LogProducer m) ()
testCase g = do
         let prefix = wait 2 >=> windowsflat 
             kl = [ contentsflat >=> textEq "foo" >=> click,
                    contentsflat >=> textEq "dialog button" >=> click,
                    menuflat >=> textEq "Menu1" >=> click,
                    popupflat >=> textEq "SubMenu1" >=> click ]
         g <- maybeifyManyK prefix return kl $ g
         logmsg "foo log message"
         g <- maybeify $ prefix >=> popupflat >=> textEq "submenuitem2" >=> toggle True $ g
         logmsg "getting a screenshot"
         i <- maybeify $ windowsflat >=> image $ g
         logimg i
         logmsg "now for a second menu"
         g <- wait 2 g
         g <- withMenuBar (wait 2 >=> windowsflat) Nothing (map (==) ["Menu1","SubMenu1","submenuitem1"]) $ g
         c <- maybeify $ windowsflat >=> contentsflat >=> textEq "foo" $ g
         logmsg "mmmmmmm"   
         g <- click c
         g <- wait 2 g
         g <- maybeify $ windowsflat >=> contentsflat >=> textEq "dialog button" >=> click $ g
         maybeify $ windowsflat >=> closew $ g
         logmsg "loggy log"   
         return ()           

main :: IO ()
main = do
  args <- getArgs 
  let addr = head args
      port = PortNumber . fromIntegral $ 26060
      endpoint = Endpoint addr port

      test:: MaybeT (LogProducer Protocol) ()
      test = liftBase getgui >>= testCase

      producer:: LogProducer Protocol (Maybe ())
      producer = runMaybeT test

      producerIO () = mapProxy (hoistFreeT runProtocol) producer 
      -- for a null logger use discard ()
      logConsumer:: MonadIO mio => () -> LogConsumer mio a
      logConsumer () = forever $ do 
            entry <- request ()
            case entry of
                TextEntry txt -> liftIO $ TIO.putStrLn txt
                ImageEntry image -> liftIO $ B.writeFile "/tmp/loggedxanelaimage.png" image
      eerio = runProxy $ producerIO >-> logConsumer
  r <- flip runReaderT endpoint . runEitherT . runEitherT $ eerio
  case r of
        Left _ -> putStrLn "io error"
        Right r2 -> case r2 of
            Left _ -> putStrLn "procotol error"
            Right r3 -> case r3 of
                Nothing -> putStrLn "nothing"
                Just _ -> putStrLn "all ok, only one result"
