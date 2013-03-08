{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Prelude hiding (catch,(.),id)
import System.Environment
import Data.Monoid
import Data.Tree
import qualified Data.Text as T
import Network 
import Control.Category
import Control.Monad
import Control.Error
import Control.Applicative
import Control.Proxy

import Pianola.Model.Swing.Driver

checkStatusBar :: (Monad m, ComponentLike c, Treeish (c m)) => (T.Text -> Bool) -> Pianola m LogEntry (c m) ()
checkStatusBar predicate = do
    with (descendants >=> hasName (=="status bar")) $ do
        statusText <- peek text
        logmsg $ "status text is: " <> statusText
        unless (predicate statusText) $ do
            logmsg $ "Unexpected text in status bar: " <> statusText
            pfail
    clickButtonByText (=="clear")

checkDialog :: Monad m => Pianola m LogEntry (ComponentW m) ()
checkDialog = do
    clickButtonByText (=="open dialog")
    with window $ with childWindow $ with contentsPane $ do
        clickButtonByText (=="click this")
        clickButtonByText (=="close dialog")
    checkStatusBar (=="clicked button in dialog")

checkDelayedDialog :: Monad m => Pianola m LogEntry (ComponentW m) ()
checkDelayedDialog = do
    clickButtonByText (=="open slow dialog")
    with window $ do
        pmaybe pfail $ withRetry1s 14 childWindow $ do       
            with contentsPane $ clickButtonByText (=="close dialog")
        logmsg "clicked delayed close button"
        pmaybe pfail $ retryPeek1s 14 $ missing childWindow 
    checkStatusBar (=="Performed delayed close")

expandAndCheckLeafA :: Monad m => Pianola m LogEntry (ComponentW m) ()
expandAndCheckLeafA = do
    with descendants $ do 
        poke $ \g -> do    
            Treegui forest <- return . cType $ g
            cell <- replusify >=> descendants $ forest
            descendants . renderer . rootLabel >=> hasText (=="leaf a") $ cell
            (justZ . expand . rootLabel $ cell) <*> pure True
    checkStatusBar (=="leaf a is collapsed: false")

type Test = Pianola Protocol LogEntry (GUI Protocol) ()

testCase:: Test
testCase = with mainWindow $ do
    toFront
    with contentsPane $ do 
        poke $ descendants >=> hasText (=="En un lugar de la Mancha") 
                           >=> setText "Lorem ipsum dolor sit amet"
        checkStatusBar (=="Lorem ipsum dolor sit amet")
        logmsg "testing dialog"
        checkDialog
        logmsg "dialog again, each action ralentized"
        ralentize 2 $ checkDialog 
        logmsg "dialog with delayed open and close"
        checkDelayedDialog    
        logmsg "testing right click"
        rightClickByText (=="This is a label")
        pmaybe pfail $ retryPoke1s 4 $ 
            window >=> popupItem >=> hasText (=="popupitem2") >=> click  
        checkStatusBar (=="clicked on popupitem2")
        sleep 1
        logmsg "testing checkbox"
        poke $ descendants >=> hasText (=="This is a checkbox") >=> toggle True
        checkStatusBar (=="checkbox is now true")
        logmsg "foo log message"
        with window $ selectInMenuBar (Just True) $ 
            map (==) ["Menu1","SubMenu1","submenuitem2"]
        checkStatusBar (=="checkbox in menu is now true")
        logmsg "getting a screenshot"
        with window $ logcapture
        logmsg "now for a second menu"
        autolog $ with window $ selectInMenuBar Nothing $ 
            map (==) ["Menu1","SubMenu1","submenuitem1"]
        checkStatusBar (=="clicked on submenuitem1")
        sleep 2
        logmsg "opening a file chooser"
        with (descendants >=> hasText (=="Open file chooser")) $ do
            poke click
            with window $ with childWindow $ with contentsPane $ do
                poke $ descendants >=> hasText (=="") >=> setText "/tmp/foofile.txt"   
                clickButtonByText $ \txt -> or $ map (txt==) ["Open","Abrir"]
        checkStatusBar (T.isInfixOf "foofile")
        sleep 1
        with descendants $ do 
            logmsg "working with a combo box"
            selectInComboBox (=="ccc")
        checkStatusBar (=="selected in combo: ccc")
        with descendants $ do 
            sleep 2
            selectTabByText (=="tab two")  
            sleep 2
            poke $ \g -> do
                Table ll <- return . cType $ g
                cell <- replusify . concat $ ll
                descendants . renderer >=> hasText (=="7") $ cell
                return $ clickCell cell
        checkStatusBar (=="selected index in table: 2")
        with descendants $ do 
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
        with window $ enter
        checkStatusBar (=="table value at row 1 col 1 is 77")
        with descendants $ do 
            sleep 2
            selectTabByText (=="tab JTree a")  
            logmsg "tab change"
        expandAndCheckLeafA 
        with descendants $ do 
            sleep 2
            selectTabByText (=="tab JTree b")  
            logmsg "tab change"
        expandAndCheckLeafA 
    with contentsPane $ do
        with descendants $ selectTabByText (=="labels")
        sleep 1
        poke $ labeledBy (=="label2") >=> setText "hope this works!"
        checkStatusBar (=="hope this works!")
        sleep 2 
    close

main :: IO ()
main = do
  args <- getArgs 
  let addr = head args
      port = PortNumber . fromIntegral $ 26060
      endpoint = Endpoint addr port

  r <- runEitherT $ simpleSwingDriver endpoint testCase $ screenshotStream "/tmp"
  putStrLn $ "result: " <> case r of 
     Left err -> show err
     Right _ -> "all ok" 

