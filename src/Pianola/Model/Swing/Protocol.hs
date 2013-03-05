{-# LANGUAGE FlexibleInstances #-}

module Pianola.Model.Swing.Protocol (
        snapshot
    ) where

import Prelude hiding (catch,(.),id)
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
import Control.Applicative

import Pianola.Util
import Pianola.Internal
import Pianola.Protocol
import Pianola.Model.Swing

iterget :: (Monad m, Unpackable a) => I.Iteratee B.ByteString m a 
iterget = AI.parserToIteratee get

snapshot :: Protocol (GUI Protocol)
snapshot = call [pack "snapshot"] iterget >>= hoistEither

makeAction :: T.Text -> [BL.ByteString] -> Sealed Protocol
makeAction method args = Sealed [T.pack "@" <> method] $
    call (pack method:args) iterget >>= hoistEither

instance Unpackable (Window Protocol) where
    get = Window <$> get

instance Unpackable (WindowInfo Protocol) where
    get = do
        snapid <- get::Parser Int
        wid <- get::Parser Int
        v1 <- get
        v2 <- get
        v3 <- get
        v4 <- get
        v5 <- get
        let packedargs = map pack [snapid,wid] 
            getWindowImage = Nullipotent $
                call (pack "getWindowImage":packedargs) iterget >>= hoistEither
            escape = makeAction (T.pack "escape") packedargs 
            enter = makeAction (T.pack "enter") packedargs 
            closeWindow = makeAction (T.pack "closeWindow") packedargs 
            toFront = makeAction (T.pack "toFront") packedargs 
        return (WindowInfo v1 v2 v3 v4 v5 getWindowImage escape enter closeWindow toFront)

instance Unpackable (ComponentInfo Protocol) where
    get = do
        snapid <- get::Parser Int
        cid <- get::Parser Int
        v1 <- get
        v2 <- get
        v3 <- get
        v4 <- get
        v5 <- get
        v6 <- get
        v7 <- get
        let rightClick = makeAction (T.pack  "rightClick") [pack snapid, pack cid]
        return (ComponentInfo v1 v2 v3 v4 v5 v6 v7 rightClick)

instance Unpackable (Component Protocol) where
    get = Component <$> get

instance Unpackable (ComponentType Protocol) where
    get = do
        snapid <- get::Parser Int
        typeTag <- get::Parser Int
        case typeTag of
            1 -> return Panel
            2 -> do 
                v2 <- get::Parser Int
                v3 <- get
                let toggle b = makeAction (T.pack "toggle") $
                        [pack snapid, pack v2, pack b]
                return $ Toggleable v3 toggle
            3 -> do 
                v2 <- get::Parser Int
                let click = makeAction (T.pack "click") $
                        [pack snapid, pack v2]
                return $ Button click
            4 -> do
                v2 <- get::Parser (Maybe Int) 
                let setText cid txt = makeAction (T.pack "setTextField") $ 
                        [pack snapid, pack cid, pack txt] 
                return . TextField $ fmap setText v2
            5 -> return Label
            6 -> do
                cid <- get::Parser (Maybe Int) 
                let clickCombo = makeAction (T.pack "clickCombo") $
                        [pack snapid, pack cid] 
                renderer <- get 
                return $ ComboBox renderer clickCombo
            7 -> List <$> get
            8 -> Table <$> get
            9 -> Treegui <$> get
            50 -> return PopupMenu
            70 -> TabbedPane <$> get
            77 -> do
                v2 <- get
                return (Other v2)


instance Unpackable (Cell Protocol) where
    get = do
        snapid <- get::Parser Int
        componentid <- get::Parser Int
        rowid <- get::Parser Int
        columnid <- get::Parser Int
        renderer <- get
        isTreeCell <- get
        let packed3 = map pack [snapid, componentid, rowid]
            packed4 = packed3 ++ [pack columnid]
            clickCell = makeAction (T.pack "clickCell") packed4
            doubleClickCell = makeAction (T.pack "doubleClickCell") packed4
            expandCollapse b = makeAction (T.pack "expandCollapseCell") $
                packed3 ++ [pack b] 
        return $ Cell renderer clickCell doubleClickCell (guard isTreeCell *> pure expandCollapse)

instance Unpackable (Tab Protocol) where
    get = do
        snapid <- get::Parser Int
        componentid <- get::Parser Int
        tabid <- get::Parser Int
        text <- get
        tooltipMaybe <- get
        selected <- get
        let selecttab = makeAction (T.pack "selectTab" ) $
                map pack [snapid, componentid, tabid] 
        return $ Tab text tooltipMaybe selected selecttab
    
