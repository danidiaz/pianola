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

import Pianola
import Pianola.Util
import Pianola.Protocol
import Pianola.Model.Swing
import Pianola.Model.Swing.Protocol
import Pianola.Model.Swing.Combinators
import Pianola.Protocol.IO

type Test :: Pianola PianolaLog PianolaLog GUI Protocol ()

testCase1:: Test
testCase1 = 

testCase:: (Monad m, MonadBase m m) => MK GUI GUI m
testCase g = do
         let prefix = windowsflat 
         g <- threadKs (wait 2) narrowK (prefix >=> contentsflat,click) [textEq "foo", textEq "dialog button"] >=>
              logmsgK "foo log message" >=>
              withMenuBarEq prefix (Just True) ["Menu1","SubMenu1","submenuitem2"] >=>
              logmsgK "getting a screenshot"
              $g
         i <- narrowK ( windowsflat >=> image ) $ g
         logimg i
         logmsg "now for a second menu"
         g <- withMenuBarEq prefix Nothing ["Menu1","SubMenu1","submenuitem1"] >=>
              wait 2 >=>
              narrowK (windowsflat >=> contentsflat >=> textEq "foo" ) >=>
              logmsgK "mmmmmmm" >=>
              click >=>
              wait 2 >=>
              narrowK ( windowsflat >=> contentsflat >=> textEq "dialog button" >=> click ) >=>
              logmsgK "this should show the combo..." >=>
              narrowK ( windowsflat >=> contentsflat >=> clickCombo ) >=> 
              wait 2
              $g
         g <- narrow $ do candidateCell <- windowsflat >=> popupflat >=> listCell $ g
                          c <- treeflat . renderer $ candidateCell 
                          textEq "ccc" c
                          liftBase $ clickCell candidateCell  
         g <- wait 2 >=> logmsgK "Now for a change of tab" $ g
         g <- narrow $ do tab <-  windowsflat >=> contentsflat >=> tab $ g
                          logmsg . tabText $ tab -- logging inside LogicT
                          guard $ tabText tab == "tab two"  
                          liftBase $ selectTab tab   
         g <- wait 2 g
         g <- narrow $ do Table ll <- windowsflat >=> contentsflat >=> return . _componentType $ g
                          cell <- replusify . concat $ ll
                          c <- replusify . flatten . renderer $ cell
                          txt <- justZ . _text $ c
                          guard $ txt == "7" 
                          liftBase $ clickCell cell
         g <- wait 2 g
         g <- narrow $ do Table ll <- windowsflat >=> contentsflat >=> return . _componentType $ g
                          cell <- replusify . concat $ ll
                          c <- treeflat . renderer $ cell
                          txt <- justZ . _text $ c
                          guard $ txt == "4" 
                          liftBase $ doubleClickCell cell
         g <- wait 2 g
         g <- narrow $ do ct <- windowsflat >=> contentsflat' $ g
                          Table _ <- return . _componentType . rootLabel $ ct -- is it a table?
                          c <- treeflat ct -- the table's children
                          txt <- justZ._text $ c
                          guard $ txt == "4" 
                          setText "77" c
         g <- wait 2 g
         g <- narrow $ do tab <-  windowsflat >=> contentsflat >=> tab $ g
                          logmsg . tabText $ tab -- logging inside LogicT
                          guard $ tabText tab == "tab JTree a"  
                          liftBase $ selectTab tab   
         g <- wait 2 g
         g <- narrow $ do Treegui forest <- windowsflat >=> contentsflat >=> return . _componentType $ g
                          tree <- replusify forest
                          cell <- treeflat tree 
                          c <- treeflat . renderer $ cell
                          txt <- justZ . _text $ c
                          guard $ txt == "leaf a" 
                          expandf <- justZ . expand $ cell
                          liftBase $ expandf True
         g <- wait 2 g
         narrowK ( windowsflat >=> close ) >=> logmsgK "loggy log" $ g

main :: IO ()
main = do
  args <- getArgs 
  let addr = head args
      port = PortNumber . fromIntegral $ 26060
      endpoint = Endpoint addr port

      test:: MaybeT (LogProducer Protocol) (GUI Protocol)
      test = liftBase getgui >>= testCase

      producer:: LogProducer Protocol (Maybe (GUI Protocol))
      producer = runMaybeT test

      producerIO () = hoist runProtocol producer 
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
