{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pianola.Swing (
        GUIInfo (..),
        GUI,
        WindowInfo (..),
        Window,
        GUIWindow,
        ComponentInfo (..),
        ComponentType (..),
        Component,
        GUIComponent,
        Cell (..),
        Tab (..),
--        mainWindow,
--        childWindow,
--        windowTitled,
--        clickButtonByText,
--        clickButtonByToolTip,
--        rightClickByText,
--        popupItem,
--        selectInMenuBar,
--        toggleInMenuBar,
--        selectInComboBox,
--        selectTabByText, 
--        selectTabByToolTip,
--        expand,
--        labeledBy,
        Poker(..)
    ) where

import Prelude hiding (catch)
import Data.Tree
import Data.Function
import Data.Functor.Identity
import qualified Data.Text as T
import Control.Error
import Control.Monad
import Control.Comonad
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env    
import Data.List
import Data.Functor.Identity
import Pianola
import Pianola.Util
import Pianola.Geometry
import Control.Monad.Logic


data GUIInfo = GUIInfo { _snapshotId :: Int
                       , _topLevelWindows :: [Window] 
                       }

type GUI = Identity GUIInfo

data WindowInfo = WindowInfo 
    {  _windowId::Int
    ,  _windowTitle::T.Text
    ,  _windowDim::(Int,Int) 
    ,  _menu::[Component]
    ,  _popupLayer:: [Component]
    ,  _contentPane::Component
--    ,  _capture::Nullipotent m Image
--    ,  _escape::Sealed m
--    ,  _enter::Sealed m
--    ,  _close::Sealed m
--    ,  _toFront::Sealed m
    } 

type Window = Tree WindowInfo

type GUIWindow = EnvT GUI Tree WindowInfo

data ComponentInfo = ComponentInfo 
    {  _componentId :: Int 
    ,  _pos::(Int,Int)
    ,  _dim::(Int,Int)
    ,  _name::Maybe T.Text
    ,  _tooltip::Maybe T.Text
    ,  _text::Maybe T.Text
    ,  _enabled::Bool
    ,  _componentType::ComponentType
--    ,  _click::Sealed m
--    ,  _doubleClick::Sealed m
--    ,  _rightClick::Sealed m
    } 

instance Geometrical ComponentInfo where
    nwcorner = _pos
    dimensions = _dim

type Component = Tree ComponentInfo

type GUIComponent = EnvT GUIWindow Tree ComponentInfo

data ComponentType =
     Panel
    |Toggleable Bool -- (Bool -> Sealed m)
    |Button -- (Sealed m)
    |TextField Bool -- (Maybe (T.Text -> Sealed m)) -- BoolTrue if editable
    |Label
    |ComboBox (Maybe Component) -- (Sealed m)
    |List [Cell]
    |Table [[Cell]]
    |Treegui (Forest Cell) 
    |PopupMenu  
    |TabbedPane [Tab]
    |Other T.Text

data Cell = Cell 
    { _renderer::Component 
--    , _clickCell::Sealed 
--    , _doubleClickCell::Sealed 
--    , _rightClickCell::Sealed 
--    , _expand:: Maybe (Bool -> Sealed m)
    }

data Tab = Tab
    { _tabText::T.Text
    , _tabToolTip::Maybe T.Text
    , _isTabSelected:: Bool
--    , _selectTab::Sealed m
    }


--clickButton:: MonadPlus n => c m -> n (Sealed m)
--clickButton (cType -> Button a) = return a
--clickButton _ = mzero
--
--clickButtonByText :: (Monad m,ComponentLike c,Treeish (c m)) => (T.Text -> Bool) -> Glance m l (c m) (Sealed m) 
--clickButtonByText f = descendants >=> hasText f >=> clickButton

-- newtype Window m = Window { unWindow :: Tree (WindowInfo m) }

--class Windowed w where
--    window :: Monad n => w m -> n (Window m)

-- class Windowed w => WindowLike w where
--     wInfo :: w m -> WindowInfo m 
-- 
--     title :: (Monad m,Monad n) => (w m) -> n T.Text
--     title = return . _windowTitle . wInfo
-- 
--     hasTitle :: MonadPlus n => (T.Text -> Bool) -> w m -> n (w m)
--     hasTitle f w = do
--         guard . f $ _windowTitle . wInfo $ w  
--         return w
-- 
--     popupLayer :: Monad m => Glance m l (w m) (Component m)
--     popupLayer = replusify . _popupLayer . wInfo
-- 
--     logcapture :: Monad m => Pianola m LogEntry (w m) ()
--     logcapture = (peek $ liftN._capture.wInfo) >>= logimg
-- 
--     contentPane :: Monad m => Glance m l (w m) (ComponentW m)
--     contentPane win = 
--         let concrete = runIdentity $ window win
--         in return . ComponentW 
--                   . EnvT concrete
--                   . unComponent 
--                   . _contentPane   
--                   . wInfo 
--                   $ win
--     
--     toFront :: Monad m => Glance m l (w m) (Sealed m)
--     toFront = return . _toFront . wInfo
-- 
--     escape :: Monad m => Glance m l (w m) (Sealed m)
--     escape = return . _escape . wInfo
-- 
--     enter :: Monad m => Glance m l (w m) (Sealed m)
--     enter = return . _enter . wInfo
-- 
--     close :: Monad m => Glance m l (w m) (Sealed m)
--     close = return . _close . wInfo
-- 

--class ComponentLike c where
--    cInfo :: c m -> ComponentInfo m 
--
--    cType :: c m -> ComponentType m 
--    cType = _componentType . cInfo 
--
--    text :: MonadPlus n => c m -> n T.Text
--    text = justZ . _text . cInfo
--
--    hasText:: MonadPlus n => (T.Text -> Bool) -> c m -> n (c m)
--    hasText f c = do
--        t <- text $ c 
--        guard $ f t
--        return c
--
--    tooltip :: MonadPlus n => c m -> n T.Text
--    tooltip = justZ . _tooltip . cInfo
--
--    hasToolTip:: MonadPlus n => (T.Text -> Bool) -> c m -> n (c m)
--    hasToolTip f c = do
--        t <- tooltip $ c 
--        guard $ f t
--        return c
--
--    hasName:: MonadPlus n => (T.Text -> Bool) -> c m -> n (c m)
--    hasName f c = do
--        t <- justZ._name.cInfo $ c 
--        guard $ f t
--        return c
--
--    toggle:: MonadPlus n => Bool -> c m -> n (Sealed m)
--    toggle b (cType -> Toggleable _ f) = return $ f b
--    toggle _ _ = mzero
--
--    click:: Monad n => c m -> n (Sealed m)
--    click = return._click.cInfo
--
--    doubleClick:: Monad n => c m -> n (Sealed m)
--    doubleClick = return._doubleClick.cInfo
--
--    rightClick:: Monad n => c m -> n (Sealed m)
--    rightClick = return._rightClick.cInfo
--
--    clickButton:: MonadPlus n => c m -> n (Sealed m)
--    clickButton (cType -> Button a) = return a
--    clickButton _ = mzero
--
--    clickCombo:: MonadPlus n => c m -> n (Sealed m)
--    clickCombo (cType -> ComboBox _ a) = return a
--    clickCombo _ = mzero
--
--    listCellByText:: MonadPlus n => (T.Text -> Bool) -> c m -> n (Cell m)
--    listCellByText f (cType -> List l) = do 
--        cell <- replusify l
--        let renderer = _renderer cell
--        descendants >=> hasText f $ renderer
--        return cell
--    listCellByText _ _ = mzero
--
--    tableCellByText:: MonadPlus n => Int -> (T.Text -> Bool) -> c m -> n (Cell m,[Cell m])  
--    tableCellByText colIndex f (cType -> Table listOfCols) = do
--        column <- atZ listOfCols colIndex
--        (rowfocus,row) <- replusify $ zip column $ transpose listOfCols  
--        let renderer = _renderer rowfocus
--        descendants >=> hasText f $ renderer
--        return (rowfocus,row)    
--    tableCellByText _ _ _ = mzero
--
--    treeCellByText :: MonadPlus n => Int -> (T.Text -> Bool) -> c m -> n (Tree (Cell m))
--    treeCellByText depth f (cType -> Treegui cellForest) = do
--        tree <- replusify cellForest
--        level <- flip atZ depth . levels . duplicate $ tree
--        subtree <- replusify level
--        let renderer = _renderer . rootLabel $ subtree
--        descendants >=> hasText f $ renderer
--        return subtree
--    treeCellByText _ _ _ = mzero
--
--    tab:: MonadPlus n => c m -> n (Tab m)
--    tab (cType -> TabbedPane p) = replusify p
--    tab _ = mzero
--
--    setText:: MonadPlus n => T.Text -> c m -> n (Sealed m)
--    setText txt c = case (cType c) of
--        TextField (Just f) -> return $ f txt
--        _ -> mzero


--mainWindow :: Glance m l (GUI m) (Window m)
--mainWindow = replusify . _topLevelWindows . runIdentity
--
--childWindow :: Glance m l (Window m) (Window m)
--childWindow = children
--
--windowTitled :: (T.Text -> Bool) -> Glance m l (GUI m) (Window m)
--windowTitled f = mainWindow >=> descendants >=> hasTitle f 
--
--clickButtonByText :: (Monad m,ComponentLike c,Treeish (c m)) => (T.Text -> Bool) -> Glance m l (c m) (Sealed m) 
--clickButtonByText f = descendants >=> hasText f >=> clickButton
--
--clickButtonByToolTip :: (Monad m,ComponentLike c,Treeish (c m)) => (T.Text -> Bool) -> Glance m l (c m) (Sealed m) 
--clickButtonByToolTip f = descendants >=> hasToolTip f >=> clickButton
--
--rightClickByText :: (Monad m,ComponentLike c,Treeish (c m)) => (T.Text -> Bool) -> Glance m l (c m) (Sealed m) 
--rightClickByText f = descendants >=> hasText f >=> rightClick
--
--popupItem :: Monad m => Glance m l (Window m) (Component m)
--popupItem w = 
--    let insidepop = children >=> contentPane >=> descendants >=> \c -> 
--            case cType c of
--                PopupMenu -> descendants c
--                _ -> mzero
--    in (popupLayer >=> descendants $ w) `mplus` 
--       (insidepop >=> return . Component . lower . unComponentW $ w)
--
--selectInMenuBar :: Monad m => [T.Text -> Bool] -> Pianola m l (Window m) ()
--selectInMenuBar ps = 
--    let go (firstitem,middleitems,lastitem) = do
--           poke $ replusify._menu.wInfo >=> descendants >=> hasText firstitem >=> clickButton
--           let pairs = zip middleitems (clickButton <$ middleitems) ++
--                       [(lastitem, clickButton)]
--           forM_ pairs $ \(txt,action) -> 
--               pmaybe pfail $ retryPoke1s 7 $ 
--                   popupItem >=> hasText txt >=> action
--        clip l = (,,) <$> headZ l <*> (initZ l >>= tailZ) <*> lastZ l
--    in maybe pfail go (clip ps)
--
--toggleInMenuBar :: Monad m => Bool -> [T.Text -> Bool] -> Pianola m l (Window m) ()
--toggleInMenuBar toggleStatus ps = 
--    let go (firstitem,middleitems,lastitem) = do
--           poke $ replusify._menu.wInfo >=> descendants >=> hasText firstitem >=> clickButton
--           let pairs = zip middleitems (clickButton <$ middleitems) ++
--                       [(lastitem, toggle toggleStatus)]
--           forM_ pairs $ \(txt,action) -> 
--               pmaybe pfail $ retryPoke1s 7 $ 
--                   popupItem >=> hasText txt >=> action
--           replicateM_ (length pairs) $ poke escape
--        clip l = (,,) <$> headZ l <*> (initZ l >>= tailZ)  <*> lastZ l
--    in maybe pfail go (clip ps)
--
--selectInComboBox :: (Monad m, ComponentLike c, Windowed c) => (T.Text -> Bool) -> Pianola m l (c m) ()
--selectInComboBox f = do
--        poke $ clickCombo
--        poke $ window >=> popupItem >=> listCellByText f >=> return._clickCell
--
--selectTabByText :: (Monad m,ComponentLike c) => (T.Text -> Bool) -> Glance m l (c m) (Sealed m)
--selectTabByText f =  
--    tab >=> \aTab -> do    
--        guard $ f . _tabText $ aTab
--        return $ _selectTab aTab   
--
--selectTabByToolTip :: (Monad m,ComponentLike c) => (T.Text -> Bool) -> Glance m l (c m) (Sealed m)
--selectTabByToolTip f =  
--    tab >=> \aTab -> do    
--        tooltip <- justZ . _tabToolTip $ aTab
--        guard $ f tooltip
--        return $ _selectTab aTab   
--
--expand :: Monad m => Bool -> Glance m l (Tree (Cell m)) (Sealed m)
--expand b cell = (justZ . _expand . rootLabel $ cell) <*> pure b
--
--labeledBy :: (Monad m,ComponentLike c,Treeish (c m)) => (T.Text -> Bool) -> Glance m l (c m) (c m)
--labeledBy f o = do
--    ref <- descendants o 
--    Label {} <- return . cType $ ref
--    hasText f ref  
--    let 
--        positioned = sameLevelRightOf ref  
--        labellable c = case cType c of
--            Toggleable {} -> True
--            Button {} -> True
--            TextField {} -> True
--            ComboBox {} -> True
--            List {} -> True
--            Table {} -> True
--            Treegui {} -> True
--            _ -> False
--    candidates <- lift . observeAllT $ do
--        c <- descendants o 
--        guard $ labellable c && positioned c
--        return c
--    headZ $ sortBy (compare `on` minX) candidates 
--

data Poker m = Poker
    { 
    }

