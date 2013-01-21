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
import Data.Functor.Compose
import Control.Category
import Control.Error
import Control.Monad
import Control.Applicative
import Control.Monad.Base
import Control.Monad.Trans.Class
import Control.Monad.Free

import Xanela.Types
import Xanela.Util

type ProtocolF x = Compose ((,) [BL.ByteString]) (I.Iteratee B.ByteString Identity) x

type Protocol = EitherT ServerError (Free ProtocolF)

call :: [BL.ByteString] -> (I.Iteratee B.ByteString Identity x) -> Protocol x
call bs i = lift . liftF $ Compose (bs,i)

data ServerError = ObsoleteRef | InternalError

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
            gui_or_fail <- call [pack "snapshot"] (AI.parserToIteratee get)
            hoistEither gui_or_fail

instance Unpackable (WindowInfo Protocol) where
    get = do
        snapid <- get::Parser Int
        wid <- get::Parser Int
        v1 <- get
        v2 <- get
        v3 <- get
        v4 <- get
        v5 <- get
        let getWindowImage = Nullipotent $ do
                image_or_fail <- call [pack "getWindowImage", pack snapid, pack wid] (AI.parserToIteratee get)
                hoistEither image_or_fail::Protocol Image
        let escape = Sealed [] $ do
                escape_or_fail <- call [pack "escape", pack snapid, pack wid] (AI.parserToIteratee get)
                hoistEither escape_or_fail::Protocol ()
        let closeWindow = Sealed [] $ do
                close_or_fail <- call [pack "closeWindow", pack snapid, pack wid] (AI.parserToIteratee get)
                hoistEither close_or_fail::Protocol ()
        return (WindowInfo v1 v2 v3 v4 v5 getWindowImage escape closeWindow)

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
        let rightClick = Sealed [] $ do
                click_or_fail <- call [pack "rightClick", pack snapid, pack cid] (AI.parserToIteratee get)
                hoistEither click_or_fail::Protocol ()
        return (ComponentInfo v1 v2 v3 v4 v5 v6 v7 rightClick)

instance Unpackable (ComponentType Protocol) where
    get = do
        snapid <- get::Parser Int
        typeTag <- get::Parser Int
        case typeTag of
            1 -> return Panel
            2 -> do 
                v2 <- get::Parser Int
                v3 <- get
                let toggle b = Sealed [] $ do
                        toggle_or_fail <- call [pack "toggle", pack snapid, pack v2, pack (b::Bool)] (AI.parserToIteratee get)
                        hoistEither toggle_or_fail::Protocol ()
                return $ Toggleable v3 toggle
            3 -> do 
                v2 <- get::Parser Int
                let click = Sealed [] $ do
                        click_or_fail <- call [pack "click", pack snapid, pack v2] (AI.parserToIteratee get)
                        hoistEither click_or_fail::Protocol ()
                return $ Button click
            4 -> do
                v2 <- get::Parser (Maybe Int) 
                let setText cid txt = Sealed [] $ do
                        text_or_fail <- call [pack "setTextField", pack snapid, pack cid, pack txt] (AI.parserToIteratee get)
                        hoistEither text_or_fail::Protocol ()
                return . TextField $ fmap setText v2
            5 -> return Label
            6 -> do
                cid <- get::Parser (Maybe Int) 
                let clickCombo = Sealed [] $ do
                        click_or_fail <- call [pack "clickCombo", pack snapid, pack cid] (AI.parserToIteratee get)
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
        snapid <- get::Parser Int
        componentid <- get::Parser Int
        rowid <- get::Parser Int
        columnid <- get::Parser Int
        renderer <- get
        isTreeCell <- get
        let clickCell = Sealed [] $ do
                click_or_fail <- call [pack "clickCell", pack snapid, pack componentid, pack rowid, pack columnid] (AI.parserToIteratee get)
                hoistEither click_or_fail::Protocol ()
            doubleClickCell = Sealed [] $ do
                click_or_fail <- call [pack "doubleClickCell", pack snapid, pack componentid, pack rowid, pack columnid] (AI.parserToIteratee get)
                hoistEither click_or_fail::Protocol ()
            expandCollapse b = Sealed [] $ do
                expand_or_fail <- call [pack "expandCollapseCell", pack snapid, pack componentid, pack rowid, pack b] (AI.parserToIteratee get)
                hoistEither expand_or_fail::Protocol ()
            
        return $ Cell renderer clickCell doubleClickCell (guard isTreeCell *> pure expandCollapse)

instance Unpackable (Tab Protocol) where
    get = do
        snapid <- get::Parser Int
        componentid <- get::Parser Int
        tabid <- get::Parser Int
        text <- get
        tooltipMaybe <- get
        selected <- get
        let selecttab = Sealed [] $ do
                selecttab_or_fail <- call [pack "selectTab", pack snapid, pack componentid, pack tabid] (AI.parserToIteratee get)
                hoistEither selecttab_or_fail::Protocol ()
        return $ Tab text tooltipMaybe selected selecttab
    
