{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Xanela.Types.Protocol (
        ServerError (..),
        ProtocolF (..),
        Protocol (..),
        getgui
    ) where

import Prelude hiding (catch,(.),id)
import Data.MessagePack
import Data.Attoparsec.ByteString
import qualified Data.Text as T
import qualified Data.Iteratee as I
import qualified Data.Attoparsec.Iteratee as AI
import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Identity
import Control.Category
import Control.Error
import Control.Monad
import Control.Applicative
import Control.Monad.Base
import Control.Monad.Trans.Class
import Control.Monad.Free

import Xanela.Types
import Xanela.Util


data ProtocolF x = Call [BL.ByteString] (I.Iteratee B.ByteString Identity x) 
             |Delay Int x

instance MonadBase Protocol Protocol where
    liftBase = id

instance Functor ProtocolF where
    fmap f (Call bs i) = Call bs (fmap f i) 
    fmap f (Delay i x) = Delay i (f x)

data ServerError = ObsoleteRef | InternalError

type Protocol = EitherT ServerError (Free ProtocolF)

call :: [BL.ByteString] -> (I.Iteratee B.ByteString Identity x) -> Protocol x
call bs i = lift . liftF $ Call bs i

delay :: Int -> Protocol ()
delay i = lift . liftF $ Delay i ()

instance Unpackable (ServerError) where
    get = do
        tag <- get::Parser Int
        case tag of
            1 -> do
                return $ ObsoleteRef
            2 -> do 
                return $ InternalError

getgui :: Protocol (GUI Protocol)
getgui = do 
            gui_or_fail <- call [pack "get"] (AI.parserToIteratee get)
            hoistEither gui_or_fail

instance Unpackable (GUI Protocol) where
    get = do
        v1 <- get
        let delayf d = do
                            delay d
                            getgui
        return $ GUI v1 delayf

instance Unpackable (WindowInfo Protocol) where
    get = do
        xanelaid <- get::Parser Int
        wid <- get::Parser Int
        v1 <- get
        v2 <- get
        v3 <- get
        v4 <- get
        v5 <- get
        let getWindowImage = do
                image_or_fail <- call [pack "getWindowImage", pack xanelaid, pack wid] (AI.parserToIteratee get)
                hoistEither image_or_fail::Protocol Image
        let escape = do
                escape_or_fail <- call [pack "escape", pack xanelaid, pack wid] (AI.parserToIteratee get)
                hoistEither escape_or_fail::Protocol ()
                getgui
        let closeWindow = do
                close_or_fail <- call [pack "closeWindow", pack xanelaid, pack wid] (AI.parserToIteratee get)
                hoistEither close_or_fail::Protocol ()
                getgui
        return (WindowInfo v1 v2 v3 v4 v5 getWindowImage escape closeWindow)

instance Unpackable (ComponentInfo Protocol) where
    get = do
        xanelaid <- get::Parser Int
        cid <- get::Parser Int
        v1 <- get
        v2 <- get
        v3 <- get
        v4 <- get
        v5 <- get
        v6 <- get
        v7 <- get
        let rightClick = do
                click_or_fail <- call [pack "rightClick", pack xanelaid, pack cid] (AI.parserToIteratee get)
                hoistEither click_or_fail::Protocol ()
                getgui
        return (ComponentInfo v1 v2 v3 v4 v5 v6 v7 rightClick)

instance Unpackable (ComponentType Protocol) where
    get = do
        xanelaid <- get::Parser Int
        typeTag <- get::Parser Int
        case typeTag of
            1 -> return Panel
            2 -> do 
                v2 <- get::Parser Int
                v3 <- get
                let toggle b = do
                        toggle_or_fail <- call [pack "toggle", pack xanelaid, pack v2, pack (b::Bool)] (AI.parserToIteratee get)
                        hoistEither toggle_or_fail::Protocol ()
                        getgui
                return $ Toggleable v3 toggle
            3 -> do 
                v2 <- get::Parser Int
                let click = do
                        click_or_fail <- call [pack "click", pack xanelaid, pack v2] (AI.parserToIteratee get)
                        hoistEither click_or_fail::Protocol ()
                        getgui
                return $ Button click
            4 -> do
                v2 <- get::Parser (Maybe Int) 
                let setText cid txt = do
                        text_or_fail <- call [pack "setTextField", pack xanelaid, pack cid, pack txt] (AI.parserToIteratee get)
                        hoistEither text_or_fail::Protocol ()
                        getgui 
                return . TextField $ fmap setText v2
            5 -> return Label
            6 -> do
                cid <- get::Parser (Maybe Int) 
                let clickCombo = do
                        click_or_fail <- call [pack "clickCombo", pack xanelaid, pack cid] (AI.parserToIteratee get)
                        hoistEither click_or_fail::Protocol ()
                        getgui 
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
        xanelaid <- get::Parser Int
        componentid <- get::Parser Int
        rowid <- get::Parser Int
        columnid <- get::Parser Int
        renderer <- get
        isTreeCell <- get
        let clickCell = do
                click_or_fail <- call [pack "clickCell", pack xanelaid, pack componentid, pack rowid, pack columnid] (AI.parserToIteratee get)
                hoistEither click_or_fail::Protocol ()
                getgui 
            doubleClickCell = do
                click_or_fail <- call [pack "doubleClickCell", pack xanelaid, pack componentid, pack rowid, pack columnid] (AI.parserToIteratee get)
                hoistEither click_or_fail::Protocol ()
                getgui 
            expandCollapse b = do
                expand_or_fail <- call [pack "expandCollapseCell", pack xanelaid, pack componentid, pack rowid, pack b] (AI.parserToIteratee get)
                hoistEither expand_or_fail::Protocol ()
                getgui 
            
        return $ Cell renderer clickCell doubleClickCell (guard isTreeCell *> pure expandCollapse)

instance Unpackable (Tab Protocol) where
    get = do
        xanelaid <- get::Parser Int
        componentid <- get::Parser Int
        tabid <- get::Parser Int
        text <- get
        tooltipMaybe <- get
        selected <- get
        let selecttab = do
                selecttab_or_fail <- call [pack "selectTab", pack xanelaid, pack componentid, pack tabid] (AI.parserToIteratee get)
                hoistEither selecttab_or_fail::Protocol ()
                getgui 
        return $ Tab text tooltipMaybe selected selecttab
    
