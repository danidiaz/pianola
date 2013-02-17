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
        Windowed (..),
        contentsPane,
        ComponentW (..),
        Component (..),
        ComponentInfo (..),
        ComponentType (..),
        ComponentLike (..),
        Cell (..),
        Tab (..),
        mainWindow,
        childWindow,
        clickButtonByText,
        clickButtonByToolTip,
        selectInMenuBar,
        selectInComboBox,
        selectTabByText, 
        selectTabByToolTip,
        labeledBy   
    ) where

import Prelude hiding (catch)
import Data.Tree
import Data.Foldable hiding (forM_)
import Data.Traversable
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Control.Error
import Control.Error.Safe
import Control.Monad
import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Class
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env    
import Data.Foldable (toList)
import Data.List
import Pianola.Util
import Pianola.Pianola
import Pianola.Geometry
import Control.Monad.Logic

type GUI m = [Window m]

newtype Window m = Window { unWindow :: Tree (WindowInfo m) }

class Windowed w where
    window :: Monad n => w m -> n (Window m)

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

instance Windowed Window where
    window = return . id

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

instance Windowed ComponentW where
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

instance ComponentLike c => Geometrical (c m) where
    nwcorner = _pos . cInfo
    dimensions = _dim . cInfo     

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
        tabToolTip::Maybe T.Text,
        isTabSelected:: Bool,
        selectTab::Sealed m
    }

mainWindow :: Glance m l (GUI m) (Window m)
mainWindow = replusify

childWindow :: Glance m l (Window m) (Window m)
childWindow = children

clickButtonByText :: (Monad m,ComponentLike c,Treeish (c m)) => (T.Text -> Bool) -> Pianola m l (c m) () 
clickButtonByText f = poke $ descendants >=> hasText f >=> click

clickButtonByToolTip :: (Monad m,ComponentLike c,Treeish (c m)) => (T.Text -> Bool) -> Pianola m l (c m) () 
clickButtonByToolTip f = poke $ descendants >=> hasToolTip f >=> click

selectInMenuBar :: Monad m => Maybe Bool -> [T.Text -> Bool] -> Pianola m l (Window m) ()
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

selectInComboBox :: (Monad m, ComponentLike c, Windowed c) => (T.Text -> Bool) -> Pianola m l (c m) ()
selectInComboBox f = do
        poke $ clickCombo
        with window $ with popupLayer $ with descendants $ do 
            poke $ \g -> do 
                candidateCell <- listCell $ g
                descendants.renderer >=> hasText f $ candidateCell 
                return $ clickCell candidateCell  

selectTabByText :: (Monad m,ComponentLike c) => (T.Text -> Bool) -> Pianola m l (c m) ()
selectTabByText f =  
    poke $ tab >=> \aTab -> do    
        guard $ f . tabText $ aTab
        return $ selectTab aTab   

selectTabByToolTip :: (Monad m,ComponentLike c) => (T.Text -> Bool) -> Pianola m l (c m) ()
selectTabByToolTip f =  
    poke $ tab >=> \aTab -> do    
        tooltip <- justZ . tabToolTip $ aTab
        guard $ f tooltip
        return $ selectTab aTab   

labeledBy :: (Monad m,ComponentLike c,Treeish (c m)) => (T.Text -> Bool) -> Glance m l (c m) (c m)
labeledBy f o = do
    ref <- descendants o 
    Label {} <- return . cType $ ref
    hasText f ref  
    let labellable c = case cType c of
            Toggleable {} -> True
            Button {} -> True
            TextField {} -> True
            ComboBox {} -> True
            List {} -> True
            Table {} -> True
            Treegui {} -> True
            _ -> False
        positioned = sameLevelRightOf ref  
    candidates <- lift . observeAllT $ do
        c <- descendants o 
        guard $ labellable c && positioned c
        return c
    headZ $ sortBy minXcmp candidates 



