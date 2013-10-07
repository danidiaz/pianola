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
import Control.Monad.Error
import Control.Applicative

import Pianola
import Pianola.Util
import Pianola.Driver
import Pianola.Swing
import Pianola.Swing.Protocol (snapshot,remote)

import System.Environment
import System.Exit (exitFailure)

checkStatusBar :: Monad m => Remote m -> (T.Text -> Bool) -> Pianola m LogEntry GUIComponent ()
checkStatusBar p predicate = do
    with (descendants >>> prune (the.name._Just) (=="status bar")) $ do
        statusText <- peek $ fromFold (the.text._Just)
        logmsg $ "status text is: " <> statusText
        unless (predicate statusText) $ do
            logmsg $ "Unexpected text in status bar: " <> statusText
            pfail
    poke $ clickButtonByText p (=="clear")

checkDialog :: Monad m => Remote m -> Pianola m LogEntry GUIComponent ()
checkDialog p = do
    poke $ clickButtonByText p (=="open dialog")
    with (context >>> descendants1 >>> decorate (the.contentPane)) $ do
        poke $ clickButtonByText p (=="click this")
        poke $ clickButtonByText p (=="close dialog")
    checkStatusBar p (=="clicked button in dialog")

checkDelayedDialog :: Monad m => Remote m -> Pianola m LogEntry GUIComponent ()
checkDelayedDialog p = do
    poke $ clickButtonByText p (=="open slow dialog")
    with context $ do
        pmaybe pfail $ withRetry1s 7 descendants1 $ do       
            with (decorate $ the.contentPane) $ poke $ clickButtonByText p (=="close dialog")
        logmsg "clicked delayed close button"
        pmaybe pfail $ retryPeek1s 7 $ missing descendants1
    checkStatusBar p (=="Performed delayed close")

expandAndCheckLeafA :: Monad m => Remote m -> Int -> Pianola m LogEntry GUIComponent ()
expandAndCheckLeafA p depth = do
    with descendants $ do 
        -- poke $ treeCellByText depth (=="leaf a") >>> expand p True
        poke $ (decorate $ the.componentType._Treegui.folded) >>> 
               descendantsN depth >>>
               prune (the.renderer.folded.text._Just) (=="leaf a") >>> expand p True
    checkStatusBar p (=="leaf a is collapsed: false")

testCase:: Monad m => Remote m -> Pianola m LogEntry GUI () 
testCase p = with (decorate $ topLevel.folded) $ do
    poke $ toFront p
    with (decorate $ the.contentPane) $ do 
        poke $ descendants >>> prune (the.text._Just) (=="En un lugar de la Mancha") 
                           >>> setText p "Lorem ipsum dolor sit amet" 
        checkStatusBar p (=="Lorem ipsum dolor sit amet")
        logmsg "testing dialog"
        checkDialog p
        logmsg "dialog again, each action ralentized"
        ralentize 2 $ checkDialog p
        logmsg "dialog with delayed open and close"
        checkDelayedDialog p   
        logmsg "testing right click"
        poke $ descendants >>> prune (the.text._Just) (=="click dbl click") >>> click p
        checkStatusBar p (=="clicked on label")
        poke $ descendants >>> prune (the.text._Just) (=="click dbl click") >>> doubleClick p
        checkStatusBar p (=="double-clicked on label") 
        poke $ rightClickByText p (=="This is a label")
        pmaybe pfail $ retryPoke1s 4 $ 
            context >>> popupItem >>> prune (the.text._Just) (=="popupitem2") >>> clickButton p 
        checkStatusBar p (=="clicked on popupitem2")
        sleep 1
        logmsg "testing checkbox"
        poke $ descendants >>> prune (the.text._Just) (=="This is a checkbox") >>> toggle p True
        checkStatusBar p (=="checkbox is now true") 
        logmsg "foo log message"
        with context $ toggleInMenuBar p True $ 
            map (==) ["Menu1","SubMenu1","submenuitem2"]
        checkStatusBar p (=="checkbox in menu is now true") 
        logmsg "getting a screenshot"
        with context $ logcapture p 
        logmsg "now for a second menu"
        autolog $ with context $ selectInMenuBar p $ 
            map (==) ["Menu1","SubMenu1","submenuitem1"]
        checkStatusBar p (=="clicked on submenuitem1") 
        sleep 2
        logmsg "opening a file chooser"
        with (descendants >>> prune (the.text._Just) (=="Open file chooser")) $ do
            poke $ clickButton p
            with context $ with descendants1 $ with (decorate $ the.contentPane) $ do
                poke $ descendants >>> prune (the.text._Just) (=="") >>> setText p "/tmp/foofile.txt"   
                poke $ clickButtonByText p $ \txt -> or $ map (txt==) ["Open","Abrir"]
        checkStatusBar p (T.isInfixOf "foofile")
        sleep 1
        with descendants $ do 
            logmsg "working with a combo box"
            selectInComboBox p (=="ccc")
            sleep 1
        checkStatusBar p (=="selected in combo: ccc")
        with descendants $ do 
            sleep 2
            poke $ selectTabByText p (=="tab two")  
            sleep 2
            poke $ tableCellByText 2 (=="7") >>> clickCell p
        checkStatusBar p (=="selected index in table: 2")
        with descendants $ do 
            sleep 2
            poke $ tableCellByText 1 (=="4") >>> doubleClickCell p
            sleep 2
            poke $ prune (the.componentType._Table) (const True) >>>
                   descendants1 >>> prune (the.text._Just) (=="4") >>> setText p "77"  
        with context $ poke $ enter p
        checkStatusBar p (=="table value at row 1 col 1 is 77")
        with descendants $ do 
            sleep 2
            poke $ selectTabByText p (=="tab JTree a")  
            logmsg "tab change"
        expandAndCheckLeafA p 1
        with descendants $ do 
            sleep 2
            poke $ selectTabByText p (=="tab JTree b")  
            logmsg "tab change"
        expandAndCheckLeafA p 0
    with (decorate $ the.contentPane) $ do
        with descendants $ poke $ selectTabByText p (=="labels")
        sleep 1
        poke $ labeledBy (=="label2") >>> setText p "hope this works!"
        checkStatusBar p (=="hope this works!")
        sleep 2 
    poke $ close p

main :: IO ()
main = do
  args <- getArgs 
  let addr = case args of 
        [] -> "127.0.0.1"
        x:_ -> x
      port = PortNumber . fromIntegral $ 26060
      endpoint = Endpoint addr port

  r <- runErrorT $ drive snapshot endpoint (testCase remote) $ screenshotStream "dist/test"
  case r of
     Left err -> do
        putStrLn $ "result: " <> show err
        exitFailure
     Right _ -> putStrLn $ "result: all ok" 
