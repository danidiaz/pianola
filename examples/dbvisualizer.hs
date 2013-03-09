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

testCase:: String -> Test
testCase jarpath = do
    with (windowTitled (T.isInfixOf "DbVisualizer")) $ do
        toFront
        withMaybe (children >=> contentsPane) $ clickButtonByText (=="Cancel")
        selectInMenuBar Nothing $ map (==) ["Tools","Driver Manager..."]
        sleep 1
    with (windowTitled (=="Driver Manager")) $ with contentsPane $ do
        logmsg "Opened Driver Manager"
        logmsg "Selecting Mysql"
        with descendants $ poke $ \c -> do
            Table cells <- return . cType $ c
            cell <- replusify . concat $ cells
            txt <- descendants._renderer >=> text $ cell
            guard $ txt == "MySQL"
            return . _clickCell $ cell
        sleep 2
        clickButtonByToolTip (T.isInfixOf "Open file")
        with window $ with childWindow $ with contentsPane $ do
            poke $ descendants >=> hasText (=="") >=> setText "C:/Users/ESDPC/Downloads/mysql-connector-java-5.1.23-bin.jar"
            sleep 2
            clickButtonByText (=="Abrir")
            sleep 2
        with window $ close 
    return ()

main :: IO ()
main = do
  args <- getArgs 
  case args of 
     addr : jarpath : _ -> do
        let port = PortNumber . fromIntegral $ 26060
            endpoint = Endpoint addr port
        r <- runEitherT $ simpleSwingDriver endpoint (testCase jarpath) $ screenshotStream "."
        case r of
           Left err -> do
              putStrLn $ "result: " <> show err
              exitFailure
           Right _ -> putStrLn $ "result: all ok" 
     _ -> putStrLn "Required args: host jarpath"
     

