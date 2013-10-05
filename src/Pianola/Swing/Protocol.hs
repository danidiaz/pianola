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

makeComponentChange :: MonadPlus n => T.Text -> [BL.ByteString] -> GUIComponent -> n (Change Protocol)
makeComponentChange method args c = return $ Change [T.pack "@" <> method] $
    call (pack method:pack snapshotId':pack componentId':args) iterget >>= hoistEither
    where componentId' = c^.to extract.componentId
          snapshotId'  = (ask . ask $ c)^.snapshotId

makeWindowChange :: MonadPlus n => T.Text -> [BL.ByteString] -> GUIWindow -> n (Change Protocol)
makeWindowChange method args w = return $ Change [T.pack "@" <> method] $
    call (pack method:pack snapshotId':pack windowId':args) iterget >>= hoistEither
    where windowId' = w^.to extract.windowId
          snapshotId'  = (ask w)^.snapshotId

makeWindowQuery :: T.Text -> [BL.ByteString] -> GUIWindow -> Query Protocol Image
makeWindowQuery method args w = Query $
    call (pack method:pack snapshotId':pack windowId':args) iterget >>= hoistEither
    where windowId' = w^.to extract.windowId
          snapshotId' = (ask w)^.snapshotId

makeCellChange :: (Comonad c, MonadPlus n) => T.Text -> [BL.ByteString] -> EnvT GUIComponent c CellInfo -> n (Change Protocol)
makeCellChange method args cell = return $ Change [T.pack "@" <> method] $
    call (pack method:pack snapshotId':pack componentId':pack rowId':pack columnId':args) iterget >>= hoistEither
    where rowId'       = cell^.to extract.rowId 
          columnId'    = cell^.to extract.columnId 
          componentId' = (ask $ cell)^.to extract.componentId
          snapshotId'  = (ask . ask . ask $ cell)^.snapshotId

makeTabChange :: MonadPlus n => T.Text -> [BL.ByteString] -> GUITab -> n (Change Protocol)
makeTabChange method args tab = return $ Change [T.pack "@" <> method] $
    call (pack method:pack snapshotId':pack componentId':pack tabId':args) iterget >>= hoistEither
    where tabId'       = tab^.to extract.tabId
          componentId' = (ask $ tab)^.to extract.componentId
          snapshotId'  = (ask . ask . ask $ tab)^.snapshotId

instance Unpackable GUI where
    get = GUI <$> get <*> get

instance Unpackable WindowInfo where
    get = do
        wid <- get::Parser Int
        v1 <- get
        v2 <- get
        v3 <- get
        v4 <- get
        v5 <- get
        return (WindowInfo wid v1 v2 v3 v4 v5) 

instance Unpackable ComponentInfo where
    get = do
        cid <- get::Parser Int
        v1 <- get
        v2 <- get
        v3 <- get
        v4 <- get
        v5 <- get
        v6 <- get
        v7 <- get
        return (ComponentInfo cid v1 v2 v3 v4 v5 v6 v7)

instance Unpackable ComponentType where
    get = do
        typeTag <- get::Parser Int
        case typeTag of
            1 -> return Panel
            2 -> do 
                v3 <- get
                return $ Toggleable v3 -- toggle
            3 -> do 
                return $ Button -- click
            4 -> do
                v2 <- get::Parser Bool
                return . TextField $ v2
            5 -> return Label
            6 -> do
                renderer <- get 
                return $ ComboBox renderer -- clickCombo
            7 -> List <$> get
            8 -> Table <$> get
            9 -> Treegui <$> get
            50 -> return PopupMenu
            70 -> TabbedPane <$> get
            77 -> Other <$> get

instance Unpackable CellInfo where
    get = do
        rowid <- get::Parser Int
        columnid <- get::Parser Int
        renderer <- get
        isFromTree <- get::Parser Bool
        return $ CellInfo rowid columnid isFromTree renderer -- clickCell doubleClickCell rightClickCell (guard isTreeCell *> pure expandCollapse)

instance Unpackable TabInfo where
    get = do
        tabid <- get::Parser Int
        text <- get
        tooltipMaybe <- get
        selected <- get
        return $ TabInfo tabid text tooltipMaybe selected -- selecttab
    

remote :: Remote Protocol
remote = Remote (prune (the.componentType._Button) (\_->True) >=> 
                 makeComponentChange "clickButton" [])

                (makeWindowChange "toFront" []) 

                (\txt -> prune (the.componentType._TextField) id >=> 
                         makeComponentChange "setTextField" [pack txt])

                (makeComponentChange "click" []) 
                (makeComponentChange "doubleClick" []) 
                (makeComponentChange "rightClick" []) 

                (\b -> prune (the.componentType._Toggleable) (\_->True) >=> 
                       makeComponentChange "toggle" [pack b])

                (prune (the.componentType._ComboBox) (\_->True) >=> 
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
    

