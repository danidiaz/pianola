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

import Pianola
import Pianola.Util
import Pianola.Pianola.Driver
import Pianola.Model.Swing
import Pianola.Model.Swing.Protocol (snapshot)

import System.Environment
import System.Exit (exitFailure)

checkStatusBar :: (Monad m, ComponentLike c, Treeish (c m)) => (T.Text -> Bool) -> Pianola m LogEntry (c m) ()
checkStatusBar predicate = do
    with (descendants >=> hasName (=="status bar")) $ do
        statusText <- peek text
        logmsg $ "status text is: " <> statusText
        unless (predicate statusText) $ do
            logmsg $ "Unexpected text in status bar: " <> statusText
            pfail
    poke $ clickButtonByText (=="clear")

checkDialog :: Monad m => Pianola m LogEntry (ComponentW m) ()
checkDialog = do
    poke $ clickButtonByText (=="open dialog")
    with window $ with childWindow $ with contentPane $ do
        poke $ clickButtonByText (=="click this")
        poke $ clickButtonByText (=="close dialog")
    checkStatusBar (=="clicked button in dialog")

checkDelayedDialog :: Monad m => Pianola m LogEntry (ComponentW m) ()
checkDelayedDialog = do
    poke $ clickButtonByText (=="open slow dialog")
    with window $ do
        pmaybe pfail $ withRetry1s 7 childWindow $ do       
            with contentPane $ poke $ clickButtonByText (=="close dialog")
        logmsg "clicked delayed close button"
        pmaybe pfail $ retryPeek1s 7 $ missing childWindow 
    checkStatusBar (=="Performed delayed close")

expandAndCheckLeafA :: Monad m => Int -> Pianola m LogEntry (ComponentW m) ()
expandAndCheckLeafA depth = do
    with descendants $ do 
        poke $ treeCellByText depth (=="leaf a") >=> expand True
    checkStatusBar (=="leaf a is collapsed: false")

type Test = Pianola Protocol LogEntry (GUI Protocol) ()

testCase:: Test
testCase = with mainWindow $ do
    poke toFront
    with contentPane $ do 
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
        poke $ descendants >=> hasText (=="click dbl click") >=> click
        checkStatusBar (=="clicked on label")
        poke $ descendants >=> hasText (=="click dbl click") >=> doubleClick
        checkStatusBar (=="double-clicked on label")
        poke $ rightClickByText (=="This is a label")
        pmaybe pfail $ retryPoke1s 4 $ 
            window >=> popupItem >=> hasText (=="popupitem2") >=> clickButton  
        checkStatusBar (=="clicked on popupitem2")
        sleep 1
        logmsg "testing checkbox"
        poke $ descendants >=> hasText (=="This is a checkbox") >=> toggle True
        checkStatusBar (=="checkbox is now true")
        logmsg "foo log message"
        with window $ toggleInMenuBar True $ 
            map (==) ["Menu1","SubMenu1","submenuitem2"]
        checkStatusBar (=="checkbox in menu is now true")
        logmsg "getting a screenshot"
        with window $ logcapture
        logmsg "now for a second menu"
        autolog $ with window $ selectInMenuBar $ 
            map (==) ["Menu1","SubMenu1","submenuitem1"]
        checkStatusBar (=="clicked on submenuitem1")
        sleep 2
        logmsg "opening a file chooser"
        with (descendants >=> hasText (=="Open file chooser")) $ do
            poke clickButton
            with window $ with childWindow $ with contentPane $ do
                poke $ descendants >=> hasText (=="") >=> setText "/tmp/foofile.txt"   
                poke $ clickButtonByText $ \txt -> or $ map (txt==) ["Open","Abrir"]
        checkStatusBar (T.isInfixOf "foofile")
        sleep 1
        with descendants $ do 
            logmsg "working with a combo box"
            selectInComboBox (=="ccc")
            sleep 1
        checkStatusBar (=="selected in combo: ccc")
        with descendants $ do 
            sleep 2
            poke $ selectTabByText (=="tab two")  
            sleep 2
            poke $ tableCellByText 2 (=="7") >=> return._clickCell.fst
        checkStatusBar (=="selected index in table: 2")
        with descendants $ do 
            sleep 2
            poke $ tableCellByText 1 (=="4") >=> return._doubleClickCell.fst
            sleep 2
            poke $ \g -> do    
                Table {} <- return . cType $ g 
                children >=> hasText (=="4") >=> setText "77" $ g
        with window $ poke enter
        checkStatusBar (=="table value at row 1 col 1 is 77")
        with descendants $ do 
            sleep 2
            poke $ selectTabByText (=="tab JTree a")  
            logmsg "tab change"
        expandAndCheckLeafA 1
        with descendants $ do 
            sleep 2
            poke $ selectTabByText (=="tab JTree b")  
            logmsg "tab change"
        expandAndCheckLeafA 0
    with contentPane $ do
        with descendants $ poke $ selectTabByText (=="labels")
        sleep 1
        poke $ labeledBy (=="label2") >=> setText "hope this works!"
        checkStatusBar (=="hope this works!")
        sleep 2 
    poke close

main :: IO ()
main = do
  args <- getArgs 
  let addr = case args of 
        [] -> "127.0.0.1"
        x:_ -> x
      port = PortNumber . fromIntegral $ 26060
      endpoint = Endpoint addr port

  r <- runEitherT $ simpleDriver snapshot endpoint testCase $ screenshotStream "dist/test"
  case r of
     Left err -> do
        putStrLn $ "result: " <> show err
        exitFailure
     Right _ -> putStrLn $ "result: all ok" 
