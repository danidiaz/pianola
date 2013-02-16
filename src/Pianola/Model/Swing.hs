{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pianola.Model.Swing (
        GUI (..),
        Window (..),
        WindowInfo (..),
        WindowLike (..),
        contentsPane,
        ComponentW (..),
        window,
        Component (..),
        ComponentInfo (..),
        ComponentType (..),
        ComponentLike (..),
        Cell (..),
        Tab (..),
--        menuflat,
--        popupflat,
--        contentsflat,
--        contentsflat',
--        cType,
--        titled,
--        hasText,
--        hasToolTip,
--        hasName,
--        click,
--        toggle,
--        clickCombo,
--        listCell,
--        tab,
--        setText,
        mainWindow,
        childWindow,
--        contentsPane,
--        popupLayer,
--        windowComponents,
--        popupLayerComponents,
        selectInMenuBar,
        selectInComboBox
    ) where

import Prelude hiding (catch)
import Data.Tree
import Data.Foldable hiding (forM_)
import Data.Traversable
import Data.ByteString (ByteString)
import qualified Data.Text as T
--import Control.Category
import Control.Error
import Control.Error.Safe
import Control.Monad
import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Class
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env    
import Data.Sequence (ViewL(..),ViewR(..),viewl,viewr,fromList)
import Data.Foldable (toList)
import Pianola
import Pianola.Util
import Pianola.Geometry

type GUI m = [Window m]

newtype Window m = Window { unWindow :: Tree (WindowInfo m) }

class WindowLike w where
    wInfo :: w m -> WindowInfo m 

    titled :: MonadPlus n => (T.Text -> Bool) -> w m -> n (w m)
    titled f w = do
        guard . f $ _windowTitle . wInfo $ w  
        return w

    popupLayer :: Monad m => Glance m l (w m) (Component m)
    popupLayer = replusify . _popupLayer . wInfo

contentsPane :: Monad m => Glance m l (Window m) (ComponentW m)
contentsPane win = return . ComponentW 
                          . EnvT win 
                          . unComponent 
                          . _contentsPane . wInfo $ win

instance Treeish (Window m) where
    children (Window c) = children c >>= return . Window
    descendants (Window c) = descendants c >>= return . Window

instance WindowLike Window where
    wInfo = rootLabel . unWindow

data WindowInfo m = WindowInfo 
    {
        _windowTitle::T.Text,
        _windowDim::(Int,Int),
        _menu::[Component m],
        _popupLayer:: [Component m],
        _contentsPane::Component m,
        _image::Nullipotent m Image,
        _escape::Sealed m,
        _close::Sealed m
    } 

instance WindowLike WindowInfo where
    wInfo = id

newtype ComponentW m = ComponentW { unComponentW :: EnvT (Window m) Tree (ComponentInfo m) }

instance Treeish (ComponentW m) where
    children (ComponentW c) = children c >>= return . ComponentW
    descendants (ComponentW c) = descendants c >>= return . ComponentW

instance ComponentLike ComponentW where
    cInfo = rootLabel . lower . unComponentW

window :: Monad n => ComponentW m -> n (Window m)
window = return . ask . unComponentW 

newtype Component m = Component { unComponent :: Tree (ComponentInfo m) }

instance Treeish (Component m) where
    children (Component c) = children c >>= return . Component 
    descendants (Component c) = descendants c >>= return . Component

instance ComponentLike Component where
    cInfo = rootLabel . unComponent

data ComponentInfo m = ComponentInfo 
    {
        _pos::(Int,Int),
        _dim::(Int,Int),
        _name::Maybe T.Text,
        _tooltip::Maybe T.Text,
        _text::Maybe T.Text,
        _enabled::Bool,
        _componentType::ComponentType m,
        _rightClick::Sealed m
    } 

class ComponentLike c where
    cInfo :: c m -> ComponentInfo m 

    cType :: c m -> ComponentType m 
    cType = _componentType . cInfo 

    hasText:: MonadPlus n => (T.Text -> Bool) -> c m -> n (c m)
    hasText f c = do
        t <- justZ._text.cInfo $ c 
        guard $ f t
        return c

    hasToolTip:: MonadPlus n => (T.Text -> Bool) -> c m -> n (c m)
    hasToolTip f c = do
        t <- justZ._tooltip.cInfo $ c 
        guard $ f t
        return c

    hasName:: MonadPlus n => (T.Text -> Bool) -> c m -> n (c m)
    hasName f c = do
        t <- justZ._name.cInfo $ c 
        guard $ f t
        return c

    toggle:: MonadPlus n => Bool -> c m -> n (Sealed m)
    toggle b (cType -> Toggleable _ f) = return $ f b
    toggle _ _ = mzero

    click:: MonadPlus n => c m -> n (Sealed m)
    click (cType -> Button a) = return a
    click _ = mzero

    clickCombo:: MonadPlus n => c m -> n (Sealed m)
    clickCombo (cType -> ComboBox _ a) = return a
    clickCombo _ = mzero

    listCell:: MonadPlus n => c m -> n (Cell m)
    listCell (cType -> List l) = replusify l
    listCell _ = mzero

    tab:: MonadPlus n => c m -> n (Tab m)
    tab (cType -> TabbedPane p) = replusify p
    tab _ = mzero

    setText:: MonadPlus n => T.Text -> c m -> n (Sealed m)
    setText txt c = case (cType c) of
        TextField (Just f) -> return $ f txt
        _ -> mzero

instance ComponentLike ComponentInfo where
    cInfo = id

--
--instance ComponentLike ComponentW m where
--    cInfo = cInfo . lower

data ComponentType m =
     Panel
    |Toggleable Bool (Bool -> Sealed m)
    |Button (Sealed m)
    |TextField (Maybe (T.Text -> Sealed m)) -- Nothing if not editable
    |Label
    |ComboBox (Maybe (Component m)) (Sealed m)
    |List [Cell m]
    |Table [[Cell m]]
    |Treegui (Forest (Cell m))
    |PopupMenu  
    |TabbedPane [Tab m]
    |Other T.Text

data Cell m = Cell 
    {
        renderer::Component m,
        clickCell::Sealed m,
        doubleClickCell::Sealed m,
        expand:: Maybe (Bool -> Sealed m)
    }

data Tab m = Tab
    {
        tabText::T.Text,
        tabTooltip::Maybe T.Text,
        isTabSelected:: Bool,
        selectTab::Sealed m
    }

--titled:: MonadPlus m => (T.Text -> Bool) -> Window n -> m (Window n)
--titled f w = do
--    guard . f $ _windowTitle . rootLabel $ w  
--    return w

--hasText:: MonadPlus m => (T.Text -> Bool) -> Component n -> m (Component n)
--hasText f c = do
--    t <- justZ._text.rootLabel $ c 
--    guard $ f t
--    return c
 
--hasToolTip:: MonadPlus m => (T.Text -> Bool) -> Component n -> m (Component n)
--hasToolTip f c = do
--    t <- justZ._tooltip.rootLabel $ c 
--    guard $ f t
--    return c

--hasName:: MonadPlus m => (T.Text -> Bool) -> Component n -> m (Component n)
--hasName f c = do
--    t <- justZ._name.rootLabel $ c 
--    guard $ f t
--    return c

-- 
-- 
--toggle:: MonadPlus n => Bool -> Component m -> n (Sealed m)
--toggle b (_componentType.rootLabel -> Toggleable _ f) = return $ f b
--toggle _ _ = mzero
--
---- 
---- 
--click:: MonadPlus n => Component m -> n (Sealed m)
--click (_componentType.rootLabel -> Button a) = return a
--click _ = mzero
--
--doClickByText :: (Functor m, Monad m) => (T.Text -> Bool) -> Pianola m l (Component m) ()
--doClickByText txt = poke $ hasText txt >=> click

--clickCombo:: MonadPlus n => Component m -> n (Sealed m)
--clickCombo (_componentType.rootLabel -> ComboBox _ a) = return a
--clickCombo _ = mzero
--
--listCell:: MonadPlus m => Component n -> m (Cell n)
--listCell (_componentType.rootLabel -> List l) = replusify l
--listCell _ = mzero
--
--tab:: MonadPlus m => Component n -> m (Tab n)
--tab (_componentType.rootLabel -> TabbedPane p) = replusify p
--tab _ = mzero

--rightClick:: MonadPlus m => ComponentInfo n -> m ()
--rightClick = liftBase . _rightClick

--setText:: MonadPlus m => T.Text -> Component n -> m (Sealed n)
--setText txt c = case (_componentType.rootLabel $ c) of
--    TextField (Just f) -> return $ f txt
--    _ -> mzero

-- end logic helpers
--

mainWindow :: Glance m l (GUI m) (Window m)
mainWindow = replusify

childWindow :: Glance m l (Window m) (Window m)
childWindow = children

--contentsPane :: Monad m => Glance m l (Window m) (Component m)
--contentsPane = return._contentsPane.rootLabel 
--
--popupLayer :: Monad m => Glance m l (Window m) (Component m)
--popupLayer = replusify._popupLayer.rootLabel 

--windowComponents :: (Functor m, Monad m) => Glance m l (Window m) (Component m)
--windowComponents = contentsPane >=> descendants 
--
--popupLayerComponents :: (Functor m, Monad m) => Glance m l (Window m) (Component m)
--popupLayerComponents = replusify._popupLayer.rootLabel >=> descendants  

--withWindowTitled = (Functor m, Monad m) => (T.Text -> Bool) -> Pianola m l (Window m) a -> Pianola m l (Component m) a 
--withWindowTitled 

selectInMenuBar :: (Functor m,Monad m) => Maybe Bool -> [T.Text -> Bool] -> Pianola m l (Window m) ()
selectInMenuBar shouldToggleLast ps = 
    let go (firstitem,middleitems,lastitem) = do
           poke $ replusify._menu.wInfo >=> descendants >=> hasText firstitem >=> click
           let pairs = zip middleitems (click <$ middleitems) ++
                       [(lastitem, maybe click toggle shouldToggleLast)]
           forM_ pairs $ \(txt,action) -> 
               retryPoke 1 $ replicate 7 $  
                   popupLayer >=> descendants >=> hasText txt >=> action
           when (isJust shouldToggleLast) $ replicateM_ (length pairs) 
                                                  (poke $ return._escape.wInfo)
        clip l = (,,) <$> headZ l <*> (initZ l >>= tailZ)  <*> lastZ l
    in maybe pianofail go (clip ps)

selectInComboBox :: (Functor m,Monad m) => (T.Text -> Bool) -> Pianola m l (ComponentW m) ()
selectInComboBox f = do
        poke $ clickCombo
        with window $ with popupLayer $ with descendants $ do 
            poke $ \g -> do 
                candidateCell <- listCell $ g
                descendants.renderer >=> hasText f $ candidateCell 
                return $ clickCell candidateCell  

