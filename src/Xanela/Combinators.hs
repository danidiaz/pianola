{-# LANGUAGE TemplateHaskell,GeneralizedNewtypeDeriving,OverloadedStrings #-}

module Xanela.Combinators (
        window,
        contents,
        popup, 
        menu,
        text,
        click,
        toggle,
        rightClick,
        setText,
        pinpoint,
        pinhead
    ) where

import Prelude hiding (catch,(.))
import System.IO
import System.Environment
import System.Console.GetOpt
import Data.Char
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Data.Lens.Common
import Data.Lens.Template
import Data.Default
import Data.Tree
--import Data.Foldable
import Data.Traversable
import qualified Data.Text as T
import qualified Data.Iteratee as I
import qualified Data.Iteratee.IO.Handle as IH
import qualified Data.Attoparsec.Iteratee as AI
import qualified Data.ByteString.Lazy as BL
import Control.Category
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Applicative
import Control.Exception
import Network
import Blaze.ByteString.Builder
import Data.MessagePack
import Data.MessagePack.Object
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Xanela
import Control.Monad.Logic
import Debug.Trace (trace)

mplusify :: MonadPlus m => [a] -> m a
mplusify = msum . map return

window :: GUI -> Logic WindowInfo
window ws = do
    w <- mplusify ws
    mplusify $ flatten w

contents :: WindowInfo -> Logic ComponentInfo
contents = mplusify . flatten . _topc
    
popup :: WindowInfo -> Logic ComponentInfo
popup w = do 
    c <- mplusify . _popupLayer $ w
    mplusify . flatten $ c

menu :: WindowInfo -> Logic ComponentInfo
menu w = do
    c <- mplusify . _menu $ w
    mplusify . flatten $ c

text:: T.Text -> ComponentInfo -> Logic ComponentInfo
text desiredtxt c = case _text c of 
    Just txt -> do guard $ txt == desiredtxt 
                   return c
    _ -> mzero

click:: ComponentInfo -> Logic (Xanela ())
click c = case _componentType c of
    Button _ xa -> return xa
    _ -> mzero

toggle:: Bool -> ComponentInfo -> Logic (Xanela ())
toggle state c = case _componentType c of
    Button (Just state) xa -> return xa
    _ -> mzero

rightClick:: ComponentInfo -> Logic (Xanela ())
rightClick = return . _rightClick

setText:: T.Text -> ComponentInfo -> Logic (Xanela ())
setText txt c = case _componentType c of
    TextField (Just f) -> return $ f txt
    _ -> mzero

pinpoint:: [a] -> Xanela a
pinpoint [a] = return a
pinpoint _ = throwError PinpointError

pinhead:: [a] -> Xanela a
pinhead (x:xs) = return x
pinhead _ = throwError PinpointError

