{-# LANGUAGE Rank2Types #-}
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
        Image,
        image,
        closew,
        escape,
        Component (..),
        ComponentInfo (..),
        ComponentType (..),
        Cell (..),
        Tab (..),
        windowsflat,
        menuflat,
        popupflat,
        contentsflat,
        wholewindowflat,
        text,
        textEq,
        wait,
        click,
        clickCombo,
        cell,
        tab,
        toggle,
        rightClick,
        setText
    ) where

import Prelude hiding (catch,(.))
import Data.Tree
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Control.Category
import Control.Error
import Control.Monad
import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Class

import Xanela.Util

data GUI m = GUI
    {
        _gui::[Window m],
        _wait4changes::Int -> GUIAction m
    }

type GUIAction m = m (GUI m)

type Window m = Tree (WindowInfo m)

type Image = ByteString

data WindowInfo m = WindowInfo 
    {
        _windowTitle::T.Text,
        _windowDim::(Int,Int),
        _menu::[Component m],
        _popupLayer:: [Component m],
        _topc::Component m,
        _image::m Image,
        _escape::GUIAction m,
        _close::GUIAction m
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
    |Toggleable Bool (Bool -> GUIAction m)
    |Button (GUIAction m)
    |TextField (Maybe (T.Text -> GUIAction m)) -- Nothing if not editable
    |Label
    |ComboBox (Maybe (Component m)) (GUIAction m)
    |GUIList [Cell m]
    |PopupMenu  
    |TabbedPane [Tab m]
    |Other T.Text

data Cell m = Cell 
    {
        renderer::Component m,
        select::GUIAction m
    }

data Tab m = Tab
    {
        tabText::T.Text,
        tabTooltip::Maybe T.Text,
        isTabSelected:: Bool,
        tabSelect::GUIAction m
    }

-- logic helpers
windowsflat:: MonadPlus m => GUI n -> m (WindowInfo n)
windowsflat = forestflat . _gui

menuflat:: MonadPlus m => WindowInfo n -> m (ComponentInfo n)
menuflat = forestflat . _menu

popupflat:: MonadPlus m => WindowInfo n -> m (ComponentInfo n)
popupflat = forestflat . _popupLayer

contentsflat:: MonadPlus m => WindowInfo n -> m (ComponentInfo n)
contentsflat =  treeflat . _topc

wholewindowflat::MonadPlus m => WindowInfo n -> m (ComponentInfo n)
wholewindowflat w = msum $ map ($w) [menuflat,popupflat,contentsflat]

text:: MonadPlus m => (T.Text -> Bool) -> ComponentInfo n -> m (ComponentInfo n)
text f c = do
    t <- justZ._text $ c 
    guard $ f t
    return c

textEq:: MonadPlus m => T.Text -> ComponentInfo n -> m (ComponentInfo n)
textEq t = text $ (==) t

image::MonadBase n m => WindowInfo n -> m Image
image = liftBase . _image

closew::MonadBase n m => WindowInfo n -> m (GUI n)
closew = liftBase . _close

escape::MonadBase n m => WindowInfo n -> m (GUI n)
escape = liftBase . _escape

wait::MonadBase n m => Int -> GUI n -> m (GUI n)
wait i = liftBase . flip _wait4changes i

toggle:: (MonadBase n m, MonadPlus m) => Bool -> ComponentInfo n -> m (GUI n)
toggle b (_componentType -> Toggleable _ a) = liftBase . a $ b
toggle _ _ = mzero

click:: (MonadBase n m, MonadPlus m) => ComponentInfo n -> m (GUI n)
click (_componentType -> Button a) = liftBase a
click _ = mzero

clickCombo:: (MonadBase n m, MonadPlus m) => ComponentInfo n -> m (GUI n)
clickCombo (_componentType -> ComboBox _ a) = liftBase a
clickCombo _ = mzero

cell:: (MonadBase n m, MonadPlus m) => ComponentInfo n -> m (Cell n)
cell (_componentType -> GUIList l) = replusify l
cell _ = mzero

tab:: (MonadBase n m, MonadPlus m) => ComponentInfo n -> m (Tab n)
tab (_componentType -> TabbedPane p) = replusify p
tab _ = mzero

rightClick:: (MonadBase n m) => ComponentInfo n -> m (GUI n)
rightClick = liftBase . _rightClick

setText:: (MonadBase n m, MonadPlus m) => T.Text -> ComponentInfo n -> m (GUI n)
setText txt c = case _componentType c of
    TextField (Just f) -> liftBase . f $ txt
    _ -> mzero
-- end logic helpers
