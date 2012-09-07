{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Xanela.Types (
        GUI (..),
        GUIAction,
        Window (..),
        WindowInfo (..),
        Component (..),
        ComponentInfo (..),
        ComponentType (..),
        mapGUI,
        windowsflat,
        menuflat,
        popupflat,
        contentsflat,
        text,
        click
    ) where

import Prelude hiding (catch,(.))
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
import qualified Data.ByteString.Lazy as BL
import Control.Category
import Control.Error
import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Control.Exception
import Network
import Blaze.ByteString.Builder
import Data.MessagePack
import Data.MessagePack.Object
import Control.Concurrent
import Control.Monad
import Control.Monad.Base
import Control.Monad.Logic
import Control.Monad.Trans
import Xanela.Util
import Debug.Trace (trace)

data GUI m = GUI
    {
        _gui::[Window m],
        _wait4changes::Int -> GUIAction m
    }

type GUIAction m = m (GUI m)

type Window m = Tree (WindowInfo m)

data WindowInfo m = WindowInfo 
    {
        _windowTitle::T.Text,
        _windowDim::(Int,Int),
        _menu::[Component m],
        _popupLayer:: [Component m],
        _topc::Component m
    } 

type Component m = Tree (ComponentInfo m)

data ComponentInfo m = ComponentInfo 
    {
        _pos::(Int,Int),
        _dim::(Int,Int),
        _name::Maybe T.Text,
        _tooltip::Maybe T.Text,
        _text::Maybe T.Text,
        _enabled::Bool,
        _componentType::ComponentType m,
        _rightClick::GUIAction m
    } 

data ComponentType m =
     Panel
    |Button (Maybe Bool) (GUIAction m)
    |TextField (Maybe (T.Text -> GUIAction m))
    |Label
    |PopupMenu  
    |Other T.Text

mapGUI:: (Monad m, Monad n) => (forall a.m a -> n a) -> GUI m -> GUI n
mapGUI mf = 
    let mapGUIAction = mf . liftM mapGUI'   
        mapGUI' gr = gr
            {
                _gui = fmap mapWindow' $ _gui gr,
                _wait4changes = fmap mapGUIAction $ _wait4changes gr
            }
        mapWindow' = fmap mapWindowInfo'
        mapWindowInfo' wi = wi
            {
                _menu =  fmap mapComponent' $ _menu wi,
                _popupLayer = fmap mapComponent' $ _popupLayer wi,
                _topc =  mapComponent' $ _topc wi
            }
        mapComponent' = fmap mapComponentInfo'
        mapComponentInfo' ci = ci
            {
                _componentType = mapComponentType' $ _componentType ci,
                _rightClick = mapGUIAction $ _rightClick ci
            }
        mapComponentType' Panel = Panel
        mapComponentType' ( Button m a ) = Button m $ mapGUIAction a
        mapComponentType' ( TextField m ) = TextField $ (liftM.fmap $ mapGUIAction) m
        mapComponentType' Label = Label
        mapComponentType' PopupMenu = PopupMenu
        mapComponentType' ( Other t ) = Other t
    in mapGUI'

-- logic helpers
windowsflat:: MonadPlus m => GUI n -> m (WindowInfo n)
windowsflat = forestflat . _gui

menuflat:: MonadPlus m => WindowInfo n -> m (ComponentInfo n)
menuflat = forestflat . _menu

popupflat:: MonadPlus m => WindowInfo n -> m (ComponentInfo n)
popupflat = forestflat . _popupLayer

contentsflat:: MonadPlus m => WindowInfo n -> m (ComponentInfo n)
contentsflat =  treeflat . _topc

text:: MonadPlus m => T.Text -> ComponentInfo n -> m (ComponentInfo n)
text t c = do
    t' <- justZ._text $ c 
    guard $ t == t'
    return c

click:: (MonadBase n m, MonadPlus m) => ComponentInfo n -> m (GUI n)
click (_componentType -> Button _ a) = liftBase a
click _ = mzero
-- end logic helpers
