{-# LANGUAGE TemplateHaskell,OverloadedStrings  #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Prelude hiding (catch)
import System.IO
import System.Environment
import System.Console.GetOpt
import Data.Char
import qualified Data.Map as M
import Data.List
import Data.Default
import Data.Tree
import qualified Data.Text as T
import qualified Data.Iteratee as I
import qualified Data.Iteratee.IO.Handle as IH
import qualified Data.Attoparsec.Iteratee as AI
import qualified Data.ByteString.Lazy as BL
import Control.Error
import Control.Monad
import Control.Applicative
import Control.Exception
import Network
import Data.MessagePack
import Data.MessagePack.Object
import Control.Concurrent
import Control.Monad
import Control.Monad.Base
import Control.Monad.Free
import Control.Monad.Trans.Free
import Control.Monad.Reader
import Control.Monad.Logic
import Control.Error
import Xanela.Types
import Xanela.Types.Protocol
import Xanela.Types.Protocol.IO
import Debug.Trace (trace)
 
type Search m = LogicT (EitherT AssertError m)

data AssertError = AssertError T.Text

instance MonadBase b m => MonadBase b (Search m) where
    liftBase = lift.lift.liftBase

type TestCase = MonadBase n (Search m) => Search m (GUI n) -> Search m () 

testCase:: TestCase
testCase g = g >>= wait t
               >>= windowsflat >>= contentsflat >>= text "foo" >>= click 
               >>= wait t
               >>= windowsflat >>= contentsflat >>= text "dialog button" >>= click 
               >>= wait t
               >>= windowsflat >>= menuflat >>= text "Menu1" >>= click
               >>= wait t
               >>= windowsflat >>= popupflat >>= text "SubMenu1" >>= click
               >>= wait t
               >>= windowsflat >>= popupflat >>= text "submenuitem2" >>= toggle False
               >>= wait t
               >>  return ()
    where t = 2

main :: IO ()
main = do
  args <- getArgs 
  let addr = head args
      port = PortNumber . fromIntegral $ 26060
      endpoint = Endpoint addr port
      p = runEitherT . observeAllT $ do
            testCase.lift.lift $ getgui 
      pio = runEitherT . runInIO . runEitherT $ p   
  r <- runReaderT pio endpoint 
  case r of
        Left _ -> putStrLn "io error"
        Right r2 -> case r2 of
            Left _ -> putStrLn "procotol error"
            Right r3 -> case r3 of
                Left _ -> putStrLn "assertion error"
                Right u -> case u of
                    [()] -> putStrLn "all ok, only one result"
                    _ -> putStrLn "oops this shouldn't happen"
--      xanelaDo x = runEitherT $ runReaderT (unXanela x) endpoint
--      xanela = do
--                 gui >>= \g -> join . liftMaybeToXanela PinpointError . tryObserveUnique $ do
--                    window g >>= contents >>= setText "foo val for text field"  
--                 gui >>= \g -> join . liftMaybeToXanela PinpointError . tryObserveUnique $ do
--                    window g >>= contents >>= text "foo" >>= click 
--                 gui >>= \g -> join . liftMaybeToXanela PinpointError . tryObserveUnique $ do
--                    window g >>= contents >>= text "dialog button" >>= click 
--
--  result <- xanelaDo xanela    
--  case result of 
--    Left err -> putStrLn . show $ err
--    Right _ -> putStrLn "All ok!"
      
--  wlist <- xanelaDo gui
--  mapM_ putStrLn strlist
--  xanelaDo $ gui >>= clickMenuWithText "submenuitem1"
--  xanelaDo $ gui >>= clickMenuWithText "submenuitem2"
--  xanelaDo $ gui >>= rightClickByText "This is a label"
--  strlist <- xanelaDo prettyPrintPopupLayer
--  mapM_ putStrLn strlist
--  xanelaDo $ gui >>= setATextField "foo val for text field"
  -- xanelaDo $ gui >>= clickButtonWithText "foo"
  -- wlist2 <- xanelaDo gui
  -- mapM_ (putStrLn . drawTree . fmap (show . _componentType) . _topc) (concatMap flatten wlist2)
      
   
