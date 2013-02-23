{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Prelude hiding (catch,(.),id)
import System.IO
import System.Environment
import System.Console.GetOpt
import Data.Tree
import qualified Data.Text as T
import Network 
import Control.Category
import Control.Error
import Control.Applicative
import Control.Proxy
import Control.Monad
import Control.Comonad
import Control.Exception
import Control.Monad.Free
--import Control.Monad.Trans.Reader
import Control.Monad.Logic
--import Control.Monad.Trans.Maybe
--import Control.Monad.Trans.Free  

import Pianola.Util
import Pianola.Pianola
import Pianola.Pianola.Driver
import Pianola.Protocol
import Pianola.Protocol.IO
import Pianola.Model.Swing
import Pianola.Model.Swing.Driver

type Test = Pianola Protocol LogEntry (GUI Protocol) ()

testCase:: Test
testCase = with mainWindow $ do
    with contentsPane $ do 
        poke $ descendants >=> hasText (=="foo") >=> click
        with window $ with childWindow $ with contentsPane $ do
            clickButtonByText (=="dialog button")
    logmsg "foo log message"
    selectInMenuBar (Just True) $ map (==) ["Menu1","SubMenu1","submenuitem2"]
    logmsg "getting a screenshot"
    (peek $ liftN._image.wInfo) >>= logimg
    logmsg "now for a second menu"
    autolog $ selectInMenuBar Nothing $ map (==) ["Menu1","SubMenu1","submenuitem1"]
    sleep 2
    ralentize 3 $ with contentsPane $ do 
        poke $ descendants >=> hasText (=="foo") >=> click
        logmsg "3 seconds before, 3 after"
        with window $ with childWindow $ with contentsPane $ do
            poke $ descendants >=> hasText (=="dialog button") >=> click
    with contentsPane $ with descendants $ do 
        logmsg "this should show the combo"
        selectInComboBox (=="ccc")
        sleep 2
        selectTabByText (=="tab two")  
        sleep 2
        poke $ \g -> do
            Table ll <- return . cType $ g
            cell <- replusify . concat $ ll
            descendants . renderer >=> hasText (=="7") $ cell
            return $ clickCell cell
        sleep 2
        poke $ \g -> do
            Table ll <- return . cType $ g
            cell <- replusify . concat $ ll
            descendants . renderer >=> hasText (=="4") $ cell
            return $ doubleClickCell cell
        sleep 2
        poke $ \g -> do    
            Table {} <- return . cType $ g 
            children >=> hasText (=="4") >=> setText "77" $ g
        sleep 2
        selectTabByText (=="tab JTree a")  
        logmsg "tab change"
        sleep 2
        poke $ \g -> do    
            Treegui forest <- return . cType $ g
            cell <- replusify >=> descendants $ forest
            descendants . renderer . rootLabel >=> hasText (=="leaf a") $ cell
            (justZ . expand . rootLabel $ cell) <*> pure True
        sleep 2
        selectTabByText (=="labels")
    with contentsPane $ do
        sleep 1
        poke $ labeledBy (=="label2") >=> setText "hope this works!"
        sleep 2 
    poke $ return . _close . wInfo

main :: IO ()
main = do
  args <- getArgs 
  let addr = head args
      port = PortNumber . fromIntegral $ 26060
      endpoint = Endpoint addr port

  r <- runEitherT $ simpleSwingDriver endpoint testCase $ screenshotStream "/tmp"
  putStrLn $ either (\_->"some error") (\_->"all ok") $ r   

