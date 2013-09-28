{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import Prelude hiding (catch,(.),id)
import Data.Monoid
import Data.Tree
import qualified Data.Text as T
import Network 
import Control.Lens
import Control.Category
import Control.Monad
import Control.Error
import Control.Applicative

import Pianola
import Pianola.Util
import Pianola.Driver
import Pianola.Swing
import Pianola.Swing.Protocol (snapshot)

import System.Environment
import System.Exit (exitFailure)

checkStatusBar :: Monad m => Poker m -> (T.Text -> Bool) -> Pianola m LogEntry GUIComponent ()
checkStatusBar p predicate = do
    with (descendants >=> which _windowTitle (=="status bar")) $ do
        statusText <- peek $ perhaps _text
        logmsg $ "status text is: " <> statusText
        unless (predicate statusText) $ do
            logmsg $ "Unexpected text in status bar: " <> statusText
            pfail
    poke $ clickButtonByText p (=="clear")

checkDialog :: Monad m => Poker m -> Pianola m LogEntry GUIComponent ()
checkDialog p = do
    poke $ clickButtonByText p (=="open dialog")
    with (the window >=> fils >=> the _contentPane) $ do
        poke $ clickButtonByText p (=="click this")
        poke $ clickButtonByText p (=="close dialog")
    checkStatusBar p (=="clicked button in dialog")

checkDelayedDialog :: Monad m => Poker m -> Pianola m LogEntry GUIComponent ()
checkDelayedDialog p = do
    poke $ clickButtonByText p (=="open slow dialog")
    with window $ do
        pmaybe pfail $ withRetry1s 7 fils $ do       
            with (the contentPane) $ poke $ clickButtonByText p (=="close dialog")
        logmsg "clicked delayed close button"
        pmaybe pfail $ retryPeek1s 7 $ missing fils
    checkStatusBar p (=="Performed delayed close")

expandAndCheckLeafA :: Monad m => Poker m -> Int -> Pianola m LogEntry GUIComponent ()
expandAndCheckLeafA p depth = do
    with descendants $ do 
        -- poke $ treeCellByText depth (=="leaf a") >=> expand p True
        poke $ ff (_Treegui.folded) >=>  
               foldr (>=>) return (repeat depth $ fils) >=>
               which (the' renderer.the' text._Just) (=="leaf a") >=> expand p True
    checkStatusBar p (=="leaf a is collapsed: false")

type Test = Pianola Protocol LogEntry (GUI Protocol) ()

testCase:: Poker m -> Test
testCase p = with (ff $ topLevel . folded) $ do
    poke $ toFront p
    with (the contentPane) $ do 
        poke $ descendants >=> which (text._Just) (=="En un lugar de la Mancha") 
                           >=> setText "Lorem ipsum dolor sit amet"
        checkStatusBar (=="Lorem ipsum dolor sit amet")
        logmsg "testing dialog"
        checkDialog
        logmsg "dialog again, each action ralentized"
        ralentize 2 $ checkDialog 
        logmsg "dialog with delayed open and close"
        checkDelayedDialog    
        logmsg "testing right click"
        poke $ descendants >=> which (text._Just) (=="click dbl click") >=> click
        checkStatusBar (=="clicked on label")
        poke $ descendants >=> which (text._Just) (=="click dbl click") >=> doubleClick
        checkStatusBar (=="double-clicked on label")
        poke $ rightClickByText (=="This is a label")
        pmaybe pfail $ retryPoke1s 4 $ 
            window >=> popupItem >=> which (text._Just) (=="popupitem2") >=> clickButton  
        checkStatusBar (=="clicked on popupitem2")
        sleep 1
        logmsg "testing checkbox"
        poke $ descendants >=> which (text._Just) (=="This is a checkbox") >=> toggle True
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
        with (descendants >=> which (text._Just) (=="Open file chooser")) $ do
            poke clickButton
            with window $ with childWindow $ with contentPane $ do
                poke $ descendants >=> which (text._Just) (=="") >=> setText "/tmp/foofile.txt"   
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
                fils >=> which (text._Just) (=="4") >=> setText "77" $ g
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

  r <- runEitherT $ drive snapshot endpoint testCase $ screenshotStream "dist/test"
  case r of
     Left err -> do
        putStrLn $ "result: " <> show err
        exitFailure
     Right _ -> putStrLn $ "result: all ok" 
