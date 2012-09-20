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
 
type Search m = MaybeT (Producer LogEntry m)

type LogEntry = T.Text 

logmsg:: (Monad m, MonadTrans t) => LogEntry -> t (Producer LogEntry m) ()
logmsg = lift . yield

testCase:: (Monad m, MonadBase n m) => GUI n -> Search m ()
testCase g = do
         let prefix = wait 2 >=> windowsflat 
             kl = [ contentsflat >=> text "foo" >=> click,
                    contentsflat >=> text "dialog button" >=> click,
                    menuflat >=> text "Menu1" >=> click,
                    popupflat >=> text "SubMenu1" >=> click ]
         g <- sandwich prefix return kl $ g
         logmsg "foo log message"
         maybeify $ prefix >=> popupflat >=> text "submenuitem2" >=> toggle g True $ g
         return ()           

main :: IO ()
main = do
  args <- getArgs 
  let addr = head args
      port = PortNumber . fromIntegral $ 26060
      endpoint = Endpoint addr port

      test:: Search Protocol ()
      test = liftBase getgui >>= testCase

      producer:: Producer LogEntry Protocol (Maybe ())
      producer = runMaybeT test

      producerIO = mapFreeT runProtocol $ producer 
      -- for a null logger use discard ()
      logConsumer = do 
            msg <- await 
            liftIO $ TIO.putStrLn msg
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
