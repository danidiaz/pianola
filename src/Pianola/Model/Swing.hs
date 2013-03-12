{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pianola.Model.Swing (
        GUI (..),
        Window (..),
        WindowInfo (..),
        WindowLike (..),
        Windowed (..),
        ComponentW (..),
        Component (..),
        ComponentInfo (..),
        ComponentType (..),
        ComponentLike (..),
        Cell (..),
        Tab (..),
        mainWindow,
        childWindow,
        windowTitled,
        clickButtonByText,
        clickButtonByToolTip,
        rightClickByText,
        popupItem,
        selectInMenuBar,
        toggleInMenuBar,
        selectInComboBox,
        selectTabByText, 
        selectTabByToolTip,
        labeledBy   
    ) where

import Prelude hiding (catch)
import Data.Tree
import Data.Function
import Data.Functor.Identity
import qualified Data.Text as T
import Control.Error
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env    
import Data.List
import Pianola.Util
import Pianola.Pianola
import Pianola.Geometry
import Control.Monad.Logic

type GUI m = [Window m]

newtype Window m = Window { unWindow :: Tree (WindowInfo m) }

class Windowed w where
    window :: Monad n => w m -> n (Window m)

class Windowed w => WindowLike w where
    wInfo :: w m -> WindowInfo m 

    title :: (Monad m,Monad n) => (w m) -> n T.Text
    title = return . _windowTitle . wInfo

    hasTitle :: MonadPlus n => (T.Text -> Bool) -> w m -> n (w m)
    hasTitle f w = do
        guard . f $ _windowTitle . wInfo $ w  
        return w

    popupLayer :: Monad m => Glance m l (w m) (Component m)
    popupLayer = replusify . _popupLayer . wInfo

    logcapture :: Monad m => Pianola m LogEntry (w m) ()
    logcapture = (peek $ liftN._capture.wInfo) >>= logimg

    contentsPane :: Monad m => Glance m l (w m) (ComponentW m)
    contentsPane win = 
        let concrete = runIdentity $ window win
        in return . ComponentW 
                  . EnvT concrete
                  . unComponent 
                  . _contentsPane   
                  . wInfo 
                  $ win
    
    toFront :: Monad m => Pianola m l (w m) ()
    toFront = poke $ return . _toFront . wInfo

    escape :: Monad m => Pianola m l (w m) ()
    escape = poke $ return . _escape . wInfo

    enter :: Monad m => Pianola m l (w m) ()
    enter = poke $ return . _enter . wInfo

    close :: Monad m => Pianola m l (w m) ()
    close = poke $ return . _close . wInfo

instance Treeish (Window m) where
    children (Window c) = children c >>= return . Window
    descendants (Window c) = descendants c >>= return . Window

instance WindowLike Window where
    wInfo = rootLabel . unWindow

instance Windowed Window where
    window = return . id

data WindowInfo m = WindowInfo 
    {  _windowTitle::T.Text
    ,  _windowDim::(Int,Int)
    ,  _menu::[Component m]
    ,  _popupLayer:: [Component m]
    ,  _contentsPane::Component m
    ,  _capture::Nullipotent m Image
    ,  _escape::Sealed m
    ,  _enter::Sealed m
    ,  _close::Sealed m
    ,  _toFront::Sealed m
    } 

newtype ComponentW m = ComponentW 
    { unComponentW :: EnvT (Window m) Tree (ComponentInfo m) }

instance Treeish (ComponentW m) where
    children (ComponentW c) = children c >>= return . ComponentW
    descendants (ComponentW c) = descendants c >>= return . ComponentW

instance ComponentLike ComponentW where
    cInfo = rootLabel . lower . unComponentW

instance Windowed ComponentW where
    window = return . ask . unComponentW 

newtype Component m = Component 
    { unComponent :: Tree (ComponentInfo m) }

instance Treeish (Component m) where
    children (Component c) = children c >>= return . Component 
    descendants (Component c) = descendants c >>= return . Component

instance ComponentLike Component where
    cInfo = rootLabel . unComponent

data ComponentInfo m = ComponentInfo 
    {  _pos::(Int,Int)
    ,  _dim::(Int,Int)
    ,  _name::Maybe T.Text
    ,  _tooltip::Maybe T.Text
    ,  _text::Maybe T.Text
    ,  _enabled::Bool
    ,  _componentType::ComponentType m
    ,  _click::Sealed m
    ,  _doubleClick::Sealed m
    ,  _rightClick::Sealed m
    } 

instance ComponentLike c => Geometrical (c m) where
    nwcorner = _pos . cInfo
    dimensions = _dim . cInfo     

class ComponentLike c where
    cInfo :: c m -> ComponentInfo m 

    cType :: c m -> ComponentType m 
    cType = _componentType . cInfo 

    text :: MonadPlus n => c m -> n T.Text
    text = justZ . _text . cInfo

    hasText:: MonadPlus n => (T.Text -> Bool) -> c m -> n (c m)
    hasText f c = do
        t <- text $ c 
        guard $ f t
        return c

    tooltip :: MonadPlus n => c m -> n T.Text
    tooltip = justZ . _tooltip . cInfo

    hasToolTip:: MonadPlus n => (T.Text -> Bool) -> c m -> n (c m)
    hasToolTip f c = do
        t <- tooltip $ c 
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

    click:: Monad n => c m -> n (Sealed m)
    click = return._click.cInfo

    doubleClick:: Monad n => c m -> n (Sealed m)
    doubleClick = return._doubleClick.cInfo

    rightClick:: Monad n => c m -> n (Sealed m)
    rightClick = return._rightClick.cInfo

    clickButton:: MonadPlus n => c m -> n (Sealed m)
    clickButton (cType -> Button a) = return a
    clickButton _ = mzero

    clickCombo:: MonadPlus n => c m -> n (Sealed m)
    clickCombo (cType -> ComboBox _ a) = return a
    clickCombo _ = mzero

    listCellByText:: MonadPlus n => (T.Text -> Bool) -> c m -> n (Cell m)
    listCellByText f (cType -> List l) = do 
        cell <- replusify l
        let renderer = _renderer cell
        descendants >=> hasText f $ renderer
        return cell
    listCellByText _ _ = mzero

    tableCellByText:: MonadPlus n => Int -> (T.Text -> Bool) -> c m -> n (Cell m,[Cell m])  
    tableCellByText colIndex f (cType -> Table listOfCols) = do
        column <- atZ listOfCols colIndex
        (rowfocus,row) <- replusify $ zip column $ transpose listOfCols  
        let renderer = _renderer rowfocus
        descendants >=> hasText f $ renderer
        return (rowfocus,row)    
    tableCellByText _ _ _ = mzero

    treeCellByText :: MonadPlus n => Int -> (T.Text -> Bool) -> c m -> n (Tree (Cell m))
    treeCellByText depth f (cType -> Treegui cellTree) = undefined
    treeCellByText _ _ _ = mzero

    tab:: MonadPlus n => c m -> n (Tab m)
    tab (cType -> TabbedPane p) = replusify p
    tab _ = mzero

    setText:: MonadPlus n => T.Text -> c m -> n (Sealed m)
    setText txt c = case (cType c) of
        TextField (Just f) -> return $ f txt
        _ -> mzero

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
    { _renderer::Component m
    , _clickCell::Sealed m
    , _doubleClickCell::Sealed m
    , _expand:: Maybe (Bool -> Sealed m)
    }

data Tab m = Tab
    { _tabText::T.Text
    , _tabToolTip::Maybe T.Text
    , _isTabSelected:: Bool
    , _selectTab::Sealed m
    }

mainWindow :: Glance m l (GUI m) (Window m)
mainWindow = replusify

childWindow :: Glance m l (Window m) (Window m)
childWindow = children

windowTitled :: (T.Text -> Bool) -> Glance m l (GUI m) (Window m)
windowTitled f = replusify >=> descendants >=> hasTitle f 

clickButtonByText :: (Monad m,ComponentLike c,Treeish (c m)) => (T.Text -> Bool) -> Glance m l (c m) (Sealed m) 
clickButtonByText f = descendants >=> hasText f >=> clickButton

clickButtonByToolTip :: (Monad m,ComponentLike c,Treeish (c m)) => (T.Text -> Bool) -> Glance m l (c m) (Sealed m) 
clickButtonByToolTip f = descendants >=> hasToolTip f >=> clickButton

rightClickByText :: (Monad m,ComponentLike c,Treeish (c m)) => (T.Text -> Bool) -> Glance m l (c m) (Sealed m) 
rightClickByText f = descendants >=> hasText f >=> rightClick

popupItem :: Monad m => Glance m l (Window m) (Component m)
popupItem w = 
    let insidepop = children >=> contentsPane >=> descendants >=> \c -> 
            case cType c of
                PopupMenu -> descendants c
                _ -> mzero
    in (popupLayer >=> descendants $ w) `mplus` 
       (insidepop >=> return . Component . lower . unComponentW $ w)

selectInMenuBar :: Monad m => [T.Text -> Bool] -> Pianola m l (Window m) ()
selectInMenuBar ps = 
    let go (firstitem,middleitems,lastitem) = do
           poke $ replusify._menu.wInfo >=> descendants >=> hasText firstitem >=> clickButton
           let pairs = zip middleitems (clickButton <$ middleitems) ++
                       [(lastitem, clickButton)]
           forM_ pairs $ \(txt,action) -> 
               pmaybe pfail $ retryPoke1s 7 $ 
                   popupItem >=> hasText txt >=> action
        clip l = (,,) <$> headZ l <*> (initZ l >>= tailZ) <*> lastZ l
    in maybe pfail go (clip ps)

toggleInMenuBar :: Monad m => Bool -> [T.Text -> Bool] -> Pianola m l (Window m) ()
toggleInMenuBar toggleStatus ps = 
    let go (firstitem,middleitems,lastitem) = do
           poke $ replusify._menu.wInfo >=> descendants >=> hasText firstitem >=> clickButton
           let pairs = zip middleitems (clickButton <$ middleitems) ++
                       [(lastitem, toggle toggleStatus)]
           forM_ pairs $ \(txt,action) -> 
               pmaybe pfail $ retryPoke1s 7 $ 
                   popupItem >=> hasText txt >=> action
           replicateM_ (length pairs) escape
        clip l = (,,) <$> headZ l <*> (initZ l >>= tailZ)  <*> lastZ l
    in maybe pfail go (clip ps)

selectInComboBox :: (Monad m, ComponentLike c, Windowed c) => (T.Text -> Bool) -> Pianola m l (c m) ()
selectInComboBox f = do
        poke $ clickCombo
        poke $ window >=> popupItem >=> listCellByText f >=> return._clickCell

selectTabByText :: (Monad m,ComponentLike c) => (T.Text -> Bool) -> Pianola m l (c m) ()
selectTabByText f =  
    poke $ tab >=> \aTab -> do    
        guard $ f . _tabText $ aTab
        return $ _selectTab aTab   

selectTabByToolTip :: (Monad m,ComponentLike c) => (T.Text -> Bool) -> Pianola m l (c m) ()
selectTabByToolTip f =  
    poke $ tab >=> \aTab -> do    
        tooltip <- justZ . _tabToolTip $ aTab
        guard $ f tooltip
        return $ _selectTab aTab   

labeledBy :: (Monad m,ComponentLike c,Treeish (c m)) => (T.Text -> Bool) -> Glance m l (c m) (c m)
labeledBy f o = do
    ref <- descendants o 
    Label {} <- return . cType $ ref
    hasText f ref  
    let 
        positioned = sameLevelRightOf ref  
        labellable c = case cType c of
            Toggleable {} -> True
            Button {} -> True
            TextField {} -> True
            ComboBox {} -> True
            List {} -> True
            Table {} -> True
            Treegui {} -> True
            _ -> False
    candidates <- lift . observeAllT $ do
        c <- descendants o 
        guard $ labellable c && positioned c
        return c
    headZ $ sortBy (compare `on` minX) candidates 



