{-# LANGUAGE TemplateHaskell,OverloadedStrings  #-}

module Main where

import Prelude hiding (catch,(.))
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
import Control.Category
import Control.Error
import Control.Monad
import Control.Applicative
import Control.Exception
import Network
import Blaze.ByteString.Builder
import Data.MessagePack
import Data.MessagePack.Object
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Logic
import Xanela
import Xanela.Combinators
import Debug.Trace (trace)
 
main :: IO ()
main = do
  args <- getArgs 
  let addr = head args
      port = PortNumber . fromIntegral $ 26060
      endpoint = Endpoint addr port
      xanelaDo x = runEitherT $ runReaderT (unXanela x) endpoint
      xanela = do
                 gui >>= \g -> join . liftMaybeToXanela PinpointError . tryObserveUnique $ do
                    window g >>= contents >>= setText "foo val for text field"  
                 gui >>= \g -> join . liftMaybeToXanela PinpointError . tryObserveUnique $ do
                    window g >>= contents >>= text "foo" >>= click 
                 gui >>= \g -> join . liftMaybeToXanela PinpointError . tryObserveUnique $ do
                    window g >>= contents >>= text "dialog button" >>= click 

  result <- xanelaDo xanela    
  case result of 
    Left err -> putStrLn . show $ err
    Right _ -> putStrLn "All ok!"
      
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
      
   
