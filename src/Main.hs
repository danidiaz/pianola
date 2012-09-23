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

import Xanela.Util
import Xanela.Types
import Xanela.Types.Combinators
import Xanela.Types.Protocol
import Xanela.Types.Protocol.IO

class XanelaLog l where
    xnllog::LogEntry -> l ()
    logmsg::T.Text -> l ()
    logimg::Image -> l ()

    logmsg = xnllog . TextEntry
    logimg = xnllog . ImageEntry

data LogEntry = TextEntry T.Text 
                |ImageEntry Image

instance Monad m => XanelaLog (Producer LogEntry m) where
    xnllog = yield

instance (Monad l, XanelaLog l) => XanelaLog (LogicT l) where
    xnllog = lift . xnllog

instance (Monad l, XanelaLog l) => XanelaLog (MaybeT l) where
    xnllog = lift . xnllog

testCase:: (Monad m, MonadBase n m) => GUI n -> MaybeT (Producer LogEntry m) ()
testCase g = do
         let prefix = wait 2 >=> windowsflat 
             kl = [ contentsflat >=> textEq "foo" >=> click,
                    contentsflat >=> textEq "dialog button" >=> click,
                    menuflat >=> textEq "Menu1" >=> click,
                    popupflat >=> textEq "SubMenu1" >=> click ]
         g <- sandwich prefix kl return $ g
         logmsg "foo log message"
         g <- maybeify $ prefix >=> popupflat >=> textEq "submenuitem2" >=> toggle g True $ g
         logmsg "getting a screenshot"
         i <- maybeify $ windowsflat >=> image $ g
         logimg i
         logmsg "now for a second menu"
         g <- wait 2 g
         g <- withMenuBarEq windowsflat ["Menu1","SubMenu1","submenuitem1"] (wait 2) $ g
         c <- maybeify $ windowsflat >=> contentsflat >=> textEq "foo" $ g
         logmsg "mmmmmmm"   
         click c
         return ()           

main :: IO ()
main = do
  args <- getArgs 
  let addr = head args
      port = PortNumber . fromIntegral $ 26060
      endpoint = Endpoint addr port

      test:: MaybeT (Producer LogEntry Protocol) ()
      test = liftBase getgui >>= testCase

      producer:: Producer LogEntry Protocol (Maybe ())
      producer = runMaybeT test

      producerIO = mapFreeT runProtocol $ producer 
      -- for a null logger use discard ()
      logConsumer = do 
            entry <- await 
            case entry of
                TextEntry txt -> liftIO $ TIO.putStrLn txt
                ImageEntry image -> liftIO $ B.writeFile "/tmp/loggedxanelaimage.png" image
            logConsumer
      eerio = runPipe $ producerIO >+> logConsumer
  r <- flip runReaderT endpoint . runEitherT . runEitherT $ eerio
  case r of
        Left _ -> putStrLn "io error"
        Right r2 -> case r2 of
            Left _ -> putStrLn "procotol error"
            Right r3 -> case r3 of
                Nothing -> putStrLn "nothing"
                Just _ -> putStrLn "all ok, only one result"
