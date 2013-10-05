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
import qualified Data.Iteratee as I
import qualified Data.Attoparsec.Iteratee as AI
import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as BL
import Control.Category
import Control.Arrow
import Control.Error
import Control.Monad
import Control.Comonad
import Control.Lens
import Control.Comonad.Trans.Env
import Control.Applicative

import Pianola.Util
import Pianola.Internal
import Pianola.Protocol
import Pianola.Swing

iterget :: (Monad m, Unpackable a) => I.Iteratee B.ByteString m a 
iterget = AI.parserToIteratee get

-- | Monadic action to obtain a local representation of the state of a remote
-- Swing GUI.
snapshot :: Protocol GUI
snapshot = call [pack ("snapshot"::T.Text)] iterget >>= hoistEither

makeComponentChange :: MonadPlus n => T.Text -> [BL.ByteString] -> Kleisli n GUIComponent (Change Protocol)
makeComponentChange method args = Kleisli $ \c -> 
        let componentId' = c^.to extract.componentId
            snapshotId'  = (ask . ask $ c)^.snapshotId
        in  return $ Change [T.pack "@" <> method] $
                call (pack method:pack snapshotId':pack componentId':args) iterget >>= hoistEither

makeWindowChange :: MonadPlus n => T.Text -> [BL.ByteString] -> Kleisli n GUIWindow (Change Protocol)
makeWindowChange method args = Kleisli $ \w -> 
        let windowId' = w^.to extract.windowId
            snapshotId'  = (ask w)^.snapshotId
        in  return $ Change [T.pack "@" <> method] $
                call (pack method:pack snapshotId':pack windowId':args) iterget >>= hoistEither

makeWindowQuery :: T.Text -> [BL.ByteString] -> GUIWindow -> Query Protocol Image
makeWindowQuery method args w = Query $
    call (pack method:pack snapshotId':pack windowId':args) iterget >>= hoistEither
    where windowId' = w^.to extract.windowId
          snapshotId' = (ask w)^.snapshotId

makeCellChange :: (Comonad c, MonadPlus n) => T.Text -> [BL.ByteString] -> Kleisli n (EnvT GUIComponent c CellInfo) (Change Protocol)
makeCellChange method args = Kleisli $ \cell -> 
        let rowId'       = cell^.to extract.rowId 
            columnId'    = cell^.to extract.columnId 
            componentId' = (ask $ cell)^.to extract.componentId
            snapshotId'  = (ask . ask . ask $ cell)^.snapshotId
        in return $ Change [T.pack "@" <> method] $
                call (pack method:pack snapshotId':pack componentId':pack rowId':pack columnId':args) iterget >>= hoistEither

makeTabChange :: MonadPlus n => T.Text -> [BL.ByteString] -> Kleisli n GUITab (Change Protocol)
makeTabChange method args = Kleisli $ \tab -> 
        let tabId'       = tab^.to extract.tabId
            componentId' = (ask $ tab)^.to extract.componentId
            snapshotId'  = (ask . ask . ask $ tab)^.snapshotId
        in  return $ Change [T.pack "@" <> method] $
                call (pack method:pack snapshotId':pack componentId':pack tabId':args) iterget >>= hoistEither

instance Unpackable GUI where
    get = GUI <$> get 
              <*> get

instance Unpackable WindowInfo where
    get = WindowInfo <$> get 
                     <*> get 
                     <*> get    
                     <*> get 
                     <*> get 
                     <*> get 

instance Unpackable ComponentInfo where
    get = ComponentInfo <$> get 
                        <*> get 
                        <*> get 
                        <*> get 
                        <*> get 
                        <*> get 
                        <*> get 
                        <*> get

instance Unpackable ComponentType where
    get = do
        typeTag <- get::Parser Int
        case typeTag of
            1 -> pure Panel
            2 -> Toggleable <$> get
            3 -> pure Button
            4 -> TextField <$> get
            5 -> pure Label
            6 -> ComboBox <$> get
            7 -> List <$> get
            8 -> Table <$> get
            9 -> Treegui <$> get
            50 -> pure PopupMenu
            70 -> TabbedPane <$> get
            77 -> Other <$> get

instance Unpackable CellInfo where
    get = CellInfo <$> get <*> get <*> get <*> get 

instance Unpackable TabInfo where
    get = TabInfo <$> get <*> get <*> get <*> get 

remote :: Remote Protocol
remote = Remote (prune (the.componentType._Button) (\_->True) >>> 
                 makeComponentChange "clickButton" [])

                (makeWindowChange "toFront" []) 

                (\txt -> prune (the.componentType._TextField) id >>> 
                         makeComponentChange "setTextField" [pack txt])

                (makeComponentChange "click" []) 
                (makeComponentChange "doubleClick" []) 
                (makeComponentChange "rightClick" []) 

                (\b -> prune (the.componentType._Toggleable) (\_->True) >>> 
                       makeComponentChange "toggle" [pack b])

                (prune (the.componentType._ComboBox) (\_->True) >>> 
                 makeComponentChange "clickCombo" []) 

                (makeTabChange "selectTab" []) 
                (makeCellChange "clickCell" [])
                (makeCellChange "doubleClickCell" [])
                (makeCellChange "rightClickCell" [])
                (\b -> makeCellChange "expandCollapseCell" [pack b])
                (makeWindowChange "escape" []) 
                (makeWindowChange "enter" []) 
                (makeWindowChange "closeWindow" []) 
                (makeWindowQuery "getWindowImage" [])
    

