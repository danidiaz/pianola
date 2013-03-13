{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import Prelude hiding (catch,(.),id)
import Data.Monoid
import Data.Tree
import qualified Data.Text as T
import Network 
import Control.Category
import Control.Monad
import Control.Error
import Control.Applicative

import Pianola.Model.Swing.Driver

import System.Environment
import System.Exit (exitFailure)

type Test = Pianola Protocol LogEntry (GUI Protocol) ()

testCase:: T.Text -> Test
testCase jarpath = do
    with (windowTitled (T.isInfixOf "DbVisualizer")) $ do
        poke $ toFront
        pokeMaybe $ children >=> contentsPane >=> clickButtonByText (=="Cancel")
        selectInMenuBar $ map (==) ["Tools","Driver Manager..."]
        sleep 1
    with (windowTitled (=="Driver Manager")) $ with contentsPane $ do
        logmsg "Opened Driver Manager"
        logmsg "Selecting MySQL"
        with descendants $ do 
            poke $ tableCellByText 0 (=="MySQL") >=> return._clickCell.fst
        sleep 2
        poke $ clickButtonByToolTip (T.isInfixOf "Open file")
        with window $ with childWindow $ with contentsPane $ do
            poke $ descendants >=> hasText (=="") >=> setText jarpath
            sleep 2
            poke $ clickButtonByText $ \txt -> or $ map (txt==) ["Open","Abrir"]
            sleep 2
        with window $ poke close 
    return ()

main :: IO ()
main = do
  args <- getArgs 
  case args of 
     addr : jarpath : _ -> do
        let port = PortNumber . fromIntegral $ 26060
            endpoint = Endpoint addr port
        r <- runEitherT $ simpleSwingDriver endpoint (testCase $ T.pack jarpath) $ screenshotStream "."
        case r of
           Left err -> do
              putStrLn $ "result: " <> show err
              exitFailure
           Right _ -> putStrLn $ "result: all ok" 
     _ -> putStrLn "Required args: host jarpath"
     

