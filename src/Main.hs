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
import Control.MFunctor
import Control.Monad
import Control.Comonad
import Control.Exception
import Control.Monad.Base
import Control.Monad.Free
import Control.Monad.Trans.Reader
import Control.Monad.Logic
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Free  
import Control.Concurrent (threadDelay)

import Pianola
import Pianola.Util
import Pianola.Protocol
import Pianola.Model.Swing
import Pianola.Model.Swing.Protocol (snapshot)
import Pianola.Protocol.IO

type Test = Pianola Protocol LogEntry (GUI Protocol) ()

testCase:: Test
testCase = with mainWindow $ do
    with contentsPane $ do 
        poke $ descendants >=> eq hasText "foo" >=> click
    with childWindow $ with contentsPane $ do
        poke $ descendants >=> eq hasText "dialog button" >=> click
    logmsg "foo log message"
    eqm selectInMenuBar ["Menu1","SubMenu1","submenuitem2"] $ Just True
    logmsg "getting a screenshot"
    (peek $ liftN._image.rootLabel) >>= logimg
    logmsg "now for a second menu"
    eqm selectInMenuBar ["Menu1","SubMenu1","submenuitem1"] Nothing
    sleep 2
    with contentsPane $ do 
        poke $ descendants >=> eq hasText "foo" >=> click
        logmsg "mmmmmmm"
        sleep 2
    with childWindow $ with contentsPane $ do
        poke $ descendants >=> eq hasText "dialog button" >=> click
    with contentsPane $ do 
        logmsg "this should show the combo"
        poke $ descendants >=> clickCombo
        sleep 2
    with popupLayer $ with descendants $ do 
        poke $ \g -> do 
            candidateCell <- listCell $ g
            descendants.renderer >=> eq hasText "ccc" $ candidateCell 
            return $ clickCell candidateCell  
        sleep 2
        logmsg "Now for a change of tab" 
    with contentsPane $ with descendants $ do 
        poke $ tab >=> \aTab -> do 
            logmsg . tabText $ aTab -- logging inside LogicT
            guard $ tabText aTab == "tab two"  
            return $ selectTab aTab   
        sleep 2
        poke $ \g -> do
            Table ll <- cType g
            cell <- replusify . concat $ ll
            descendants . renderer >=> eq hasText "7" $ cell
            return $ clickCell cell
        sleep 2
        poke $ \g -> do
            Table ll <- cType g
            cell <- replusify . concat $ ll
            descendants . renderer >=> eq hasText "4" $ cell
            return $ doubleClickCell cell
        sleep 2
        poke $ \g -> do    
            Table _ <- cType g 
            children >=> eq hasText "4" >=> setText "77" $ g
        sleep 2
        poke $ tab >=> \aTab -> do    
            logmsg . tabText $ aTab -- logging inside LogicT
            guard $ tabText aTab == "tab JTree a"  
            return $ selectTab aTab   
        sleep 2
        poke $ \g -> do    
            Treegui forest <- cType g
            cell <- anyOf >=> descendants $ forest
            descendants . renderer . rootLabel >=> eq hasText "leaf a" $ cell
            (justZ . expand . rootLabel $ cell) <*> pure True
        sleep 2
    poke $ return._close.rootLabel

delayer :: MonadIO m => Consumer ProxyFast Delay m a
delayer = forever $ request () >>= liftIO . threadDelay . (*1000000)

logger:: MonadIO mio => LogConsumer mio a
logger = forever $ do 
      entry <- request ()
      case entry of
          TextEntry txt -> liftIO $ TIO.putStrLn txt
          ImageEntry image -> liftIO $ B.writeFile "/tmp/loggedxanelaimage.png" image

main :: IO ()
main = do
  args <- getArgs 
  let addr = head args
      port = PortNumber . fromIntegral $ 26060
      endpoint = Endpoint addr port

      -- A long peeling process until we reach IO!!!
      played = play snapshot testCase
      rebased = hoist (mapMaybeT (hoist (mapMaybeT (hoist runProtocol)))) $ played
      delayed = runProxy $ const rebased >-> const delayer
      logged1 = runProxy $ (const $ runMaybeT delayed) >-> const logger
      logged2 = runProxy $ (const $ runMaybeT logged1) >-> const logger
  r <- flip runReaderT endpoint . runEitherT . runEitherT $ logged2
  let msg = case r of
        Left _ -> "io error"
        Right r2 -> case r2 of 
            Left _ -> "protocol error"
            Right r3 -> case r3 of
                Nothing -> "arrow error"
                Just r4 -> case r4 of
                    Nothing -> "pianola error"   
                    Just () -> "all ok"
  putStrLn msg

