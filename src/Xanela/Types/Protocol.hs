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

import Prelude hiding (catch)
import System.IO
import System.Environment
import System.Console.GetOpt
import Data.Char
import qualified Data.Map as M
import Data.List
import Data.Default
import Data.Tree
--import Data.Foldable
import Data.Traversable
import qualified Data.Text as T
import qualified Data.Iteratee as I
import qualified Data.Iteratee.IO.Handle as IH
import qualified Data.Attoparsec.Iteratee as AI
import Data.Attoparsec.ByteString
-- import qualified Data.ByteString as BL hiding (pack)
import Control.Error
import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Control.Exception
import Network
-- import Blaze.ByteString.Builder
import Data.MessagePack
import Data.MessagePack.Object
import Control.Concurrent
import Control.Monad
import Control.Monad.Logic
import Control.Monad.Free
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Base
import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as BL
import Xanela.Types
import Xanela.Util
import Debug.Trace (trace)

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
        xid <- get::Parser Int
        v1 <- get
        v2 <- get
        v3 <- get
        v4 <- get
        v5 <- get
        return (WindowInfo v1 v2 v3 v4 v5)

instance Unpackable (ComponentInfo Protocol) where
    get = do
        xid <- get::Parser Int
        cid <- get::Parser Int
        v1 <- get
        v2 <- get
        v3 <- get
        v4 <- get
        v5 <- get
        v6 <- get
        v7 <- get
        let rightClick = do
                click_or_fail <- call [pack "rightClick", pack xid, pack cid] (AI.parserToIteratee get)
                hoistEither click_or_fail::Protocol ()
                getgui
        return (ComponentInfo v1 v2 v3 v4 v5 v6 v7 rightClick)

instance Unpackable (ComponentType Protocol) where
    get = do
        xid <- get::Parser Int
        typeTag <- get::Parser Int
        case typeTag of
            1 -> return Panel
            2 -> do 
                v2 <- get::Parser Int
                v3 <- get
                let click = do
                        click_or_fail <- call [pack "click", pack xid, pack v2] (AI.parserToIteratee get)
                        hoistEither click_or_fail::Protocol ()
                        getgui
                return $ Button v3 click
            3 -> do
                v2 <- get::Parser (Maybe Int) 
                let setText cid txt = do
                        text_or_fail <- call [pack "setTextField", pack xid, pack cid, pack txt] (AI.parserToIteratee get)
                        hoistEither text_or_fail::Protocol ()
                        getgui 
                return . TextField $ fmap setText v2
            4 -> return Label
            5 -> return PopupMenu
            77 -> do
                v2 <- get
                return (Other v2)
