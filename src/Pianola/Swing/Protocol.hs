{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Pianola.Swing.Protocol (
        snapshot,
        remote
    ) where

import Prelude hiding (catch,(.),id)
import Data.Functor.Identity
import Data.Monoid
import Data.MessagePack
import Data.Attoparsec.ByteString
import qualified Data.Text as T
import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as BL
import Control.Category
import Control.Arrow
import Control.Monad
import Control.Comonad
import Control.Lens
import Control.Comonad.Trans.Env
import Control.Applicative
import Control.Monad.Error
import Pianola.Util
import Pianola.Internal
import Pianola.Protocol
import Pianola.Swing.Internal


-- | Monadic action to obtain a local representation of the state of a remote
-- Swing GUI.
snapshot :: Protocol GUI
snapshot = call [pack ("snapshot"::T.Text)] get >>= ErrorT . return

makeComponentChange :: MonadPlus n => T.Text -> [BL.ByteString] -> Kleisli n GUIComponent (Change Protocol)
makeComponentChange method args = Kleisli $ \c -> 
        let componentId' = c^.to extract.componentId
            snapshotId'  = (ask . ask $ c)^.snapshotId
        in  return $ Change [T.pack "@" <> method] $
                call (pack method:pack snapshotId':pack componentId':args) get >>= ErrorT . return

makeWindowChange :: MonadPlus n => T.Text -> [BL.ByteString] -> Kleisli n GUIWindow (Change Protocol)
makeWindowChange method args = Kleisli $ \w -> 
        let windowId' = w^.to extract.windowId
            snapshotId'  = (ask w)^.snapshotId
        in  return $ Change [T.pack "@" <> method] $
                call (pack method:pack snapshotId':pack windowId':args) get >>= ErrorT . return

makeWindowQuery :: T.Text -> [BL.ByteString] -> GUIWindow -> Query Protocol Image
makeWindowQuery method args w = Query $
    call (pack method:pack snapshotId':pack windowId':args) get >>= ErrorT . return
    where windowId' = w^.to extract.windowId
          snapshotId' = (ask w)^.snapshotId

makeCellChange :: (Comonad c, MonadPlus n) => T.Text -> [BL.ByteString] -> Kleisli n (EnvT GUIComponent c CellInfo) (Change Protocol)
makeCellChange method args = Kleisli $ \cell -> 
        let rowId'       = cell^.to extract.rowId 
            columnId'    = cell^.to extract.columnId 
            componentId' = (ask $ cell)^.to extract.componentId
            snapshotId'  = (ask . ask . ask $ cell)^.snapshotId
        in return $ Change [T.pack "@" <> method] $
                call (pack method:pack snapshotId':pack componentId':pack rowId':pack columnId':args) get >>= ErrorT . return

makeTabChange :: MonadPlus n => T.Text -> [BL.ByteString] -> Kleisli n GUITab (Change Protocol)
makeTabChange method args = Kleisli $ \tab -> 
        let tabId'       = tab^.to extract.tabId
            componentId' = (ask $ tab)^.to extract.componentId
            snapshotId'  = (ask . ask . ask $ tab)^.snapshotId
        in  return $ Change [T.pack "@" <> method] $
                call (pack method:pack snapshotId':pack componentId':pack tabId':args) get >>= ErrorT . return


remote :: Remote Protocol
remote = Remote (makeWindowChange "toFront" []) 

                (makeWindowChange "escape" []) 
                (makeWindowChange "enter" []) 
                (makeWindowChange "closeWindow" []) 
                (makeWindowQuery "getWindowImage" [])

                (makeComponentChange "click" []) 
                (makeComponentChange "doubleClick" []) 
                (makeComponentChange "rightClick" []) 

                (\b -> prune (the.componentType._Toggleable) (\_->True) >>> 
                       makeComponentChange "toggle" [pack b])

                (prune (the.componentType._Button) (\_->True) >>> 
                 makeComponentChange "clickButton" [])

                (prune (the.componentType._ComboBox) (\_->True) >>> 
                 makeComponentChange "clickCombo" []) 

                (\txt -> prune (the.componentType._TextField) id >>> 
                         makeComponentChange "setTextField" [pack txt])

                (makeTabChange "selectTab" []) 
                (makeCellChange "clickCell" [])
                (makeCellChange "doubleClickCell" [])
                (makeCellChange "rightClickCell" [])
                (\b -> makeCellChange "expandCollapseCell" [pack b])
    

