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
        expand,
        labeledBy   
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
    
    toFront :: Monad m => Glance m l (w m) (Sealed m)
    toFront = return . _toFront . wInfo

    escape :: Monad m => Glance m l (w m) (Sealed m)
    escape = return . _escape . wInfo

    enter :: Monad m => Glance m l (w m) (Sealed m)
    enter = return . _enter . wInfo

    close :: Monad m => Glance m l (w m) (Sealed m)
    close = return . _close . wInfo

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

-- | A component which carries a reference to the window to which it belongs.
-- See 'Windowed'.
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
    {   -- | The position of the component withing the window. 
       _pos::(Int,Int)
        -- | Width and Height.
    ,  _dim::(Int,Int)
    ,  _name::Maybe T.Text
    ,  _tooltip::Maybe T.Text
        -- | The textual value of the component.
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

    -- | Returns the component text or 'mzero' if it doesn't have any.
    text :: MonadPlus n => c m -> n T.Text
    text = justZ . _text . cInfo

    -- | If the componentes has a text and it satisfies the predicate, returns the component, otherwise 'mzero'.
    hasText:: MonadPlus n => (T.Text -> Bool) -> c m -> n (c m)
    hasText f c = do
        t <- text $ c 
        guard $ f t
        return c

    -- | Returns the component tooltip or 'mzero' if it doesn't have any.
    tooltip :: MonadPlus n => c m -> n T.Text
    tooltip = justZ . _tooltip . cInfo

    -- | If the componentes has a tooltip and it satisfies the predicate, returns the component, otherwise 'mzero'.
    hasToolTip:: MonadPlus n => (T.Text -> Bool) -> c m -> n (c m)
    hasToolTip f c = do
        t <- tooltip $ c 
        guard $ f t
        return c

    -- | If the componentes has a name and it satisfies the predicate, returns
    -- the component, otherwise 'mzero'.
    hasName:: MonadPlus n => (T.Text -> Bool) -> c m -> n (c m)
    hasName f c = do
        t <- justZ._name.cInfo $ c 
        guard $ f t
        return c

    -- | Toggles the component to the desired state if the component is
    -- toggleable, 'mzero' otherwise.
    toggle:: MonadPlus n => Bool -> c m -> n (Sealed m)
    toggle b (cType -> Toggleable _ f) = return $ f b
    toggle _ _ = mzero

    -- | Returns the click action of a component.
    click:: Monad n => c m -> n (Sealed m)
    click = return._click.cInfo

    doubleClick:: Monad n => c m -> n (Sealed m)
    doubleClick = return._doubleClick.cInfo

    rightClick:: Monad n => c m -> n (Sealed m)
    rightClick = return._rightClick.cInfo

    -- | If the component is a button returns its click action, otherwise
    -- 'mzero'.
    clickButton:: MonadPlus n => c m -> n (Sealed m)
    clickButton (cType -> Button a) = return a
    clickButton _ = mzero

    -- | If the component is a combo box returns its click action, otherwise
    -- 'mzero'.
    clickCombo:: MonadPlus n => c m -> n (Sealed m)
    clickCombo (cType -> ComboBox _ a) = return a
    clickCombo _ = mzero

    -- | If the component is a list and has a cell whose rederer's text
    -- satisfies the predicate, returns the cell, otherwise 'mzero'.
    listCellByText:: MonadPlus n => (T.Text -> Bool) -> c m -> n (Cell m)
    listCellByText f (cType -> List l) = do 
        cell <- replusify l
        let renderer = _renderer cell
        descendants >=> hasText f $ renderer
        return cell
    listCellByText _ _ = mzero

    -- | If the component is a table and has a cell at the specified column
    -- whose rederer's text satisfies the predicate, returns the cell,
    -- otherwise 'mzero'.
    tableCellByText:: MonadPlus n => Int -> (T.Text -> Bool) -> c m -> n (Cell m,[Cell m])  
    tableCellByText colIndex f (cType -> Table listOfCols) = do
        column <- atZ listOfCols colIndex
        (rowfocus,row) <- replusify $ zip column $ transpose listOfCols  
        let renderer = _renderer rowfocus
        descendants >=> hasText f $ renderer
        return (rowfocus,row)    
    tableCellByText _ _ _ = mzero

    -- | If the component is a table and has a cell at the specified depth
    -- (starting a 0 for the root) whose rederer's text satisfies the
    -- predicate, returns the cell, otherwise 'mzero'.
    treeCellByText :: MonadPlus n => Int -> (T.Text -> Bool) -> c m -> n (Tree (Cell m))
    treeCellByText depth f (cType -> Treegui cellForest) = do
        tree <- replusify cellForest
        level <- flip atZ depth . levels . duplicate $ tree
        subtree <- replusify level
        let renderer = _renderer . rootLabel $ subtree
        descendants >=> hasText f $ renderer
        return subtree
    treeCellByText _ _ _ = mzero

    -- | Returns the tabs of a component if the component is a tabbed pane, 'mzero' otherwise.
    tab:: MonadPlus n => c m -> n (Tab m)
    tab (cType -> TabbedPane p) = replusify p
    tab _ = mzero

    -- | If the component is a text field and is editable, set the text of the text field. Otherwise 'mzero'.
    setText:: MonadPlus n => T.Text -> c m -> n (Sealed m)
    setText txt c = case (cType c) of
        TextField (Just f) -> return $ f txt
        _ -> mzero

-- | Represents data specific to each subclass of Swing components.
data ComponentType m =
     Panel
 -- | A check box, either in a window or in a popup menu. The bool value is the
 -- current selection state.
    |Toggleable Bool (Bool -> Sealed m)
 -- | A button with its selection action. Menu items in popup menus are also
 -- treated as buttons.
    |Button (Sealed m)
 -- | 'Nothing' when the textfield is not editable.
    |TextField (Maybe (T.Text -> Sealed m)) 
    |Label
 -- | A combo box which may already have a selection, and offers a click action
 -- which shows the drop-down list. See 'selectInComboBox'. 
    |ComboBox (Maybe (Component m)) (Sealed m)
    |List [Cell m]
 -- | Tables are represented as lists of columns.
    |Table [[Cell m]]
 -- | A list of trees of 'Cell'. It is a list of trees instead of a single tree
 -- so that JTrees which do not show the root can be represented.
    |Treegui (Forest (Cell m)) 
 -- | In Swing, popup menus reside in the popup layer of a window or, if the
 -- popup extends beyond the window, in the contents pane of a child window
 -- created to hold the popup. See 'popupItem'.
    |PopupMenu  
    |TabbedPane [Tab m]
 -- | The text value holds the name of the class.
    |Other T.Text

-- | Complex gui components like lists, tables and trees are represented as
-- composed of cells.
--
-- Bear in mind that in Swing the renderer sub-components of a complex
-- component do /not/ count as children of the component. However, editor
-- components /do/ count as children of the component. 
--
-- A common case is to
-- double click on a table cell to activate the cell's editor, and then having
-- to look for that editor among the descendants of the table.
data Cell m = Cell 
    { 
    -- | The rendering component. Clients should not try to invoke actions on
    -- rendering components, as they are inert and only used for display
    -- purposes. 
      _renderer::Component m
    , _clickCell::Sealed m
    , _doubleClickCell::Sealed m
    , _rightClickCell::Sealed m
    -- | Always 'Nothing' for cells not belonging to trees.
    , _expand:: Maybe (Bool -> Sealed m)
    }

data Tab m = Tab
    { _tabText::T.Text
    , _tabToolTip::Maybe T.Text
    , _isTabSelected:: Bool
    , _selectTab::Sealed m
    }

-- | Returns the may window of the application. Only works properly when there
-- is only one top-level window.
mainWindow :: Glance m l (GUI m) (Window m)
mainWindow = replusify

-- | Returns the children of a window.
childWindow :: Glance m l (Window m) (Window m)
childWindow = children

-- | Returns all visible windows whose title satisfies the predicate.
windowTitled :: (T.Text -> Bool) -> Glance m l (GUI m) (Window m)
windowTitled f = replusify >=> descendants >=> hasTitle f 

-- | If the component or *any of its descendants* is a button whose text satisfies the predicate, returns the click action. Otherwise 'mzero'.
clickButtonByText :: (Monad m,ComponentLike c,Treeish (c m)) => (T.Text -> Bool) -> Glance m l (c m) (Sealed m) 
clickButtonByText f = descendants >=> hasText f >=> clickButton

clickButtonByToolTip :: (Monad m,ComponentLike c,Treeish (c m)) => (T.Text -> Bool) -> Glance m l (c m) (Sealed m) 
clickButtonByToolTip f = descendants >=> hasToolTip f >=> clickButton

rightClickByText :: (Monad m,ComponentLike c,Treeish (c m)) => (T.Text -> Bool) -> Glance m l (c m) (Sealed m) 
rightClickByText f = descendants >=> hasText f >=> rightClick

-- | Returns all the visible popup items belonging to a window (that is, not
-- only the popups compoenents themselves, but all their options as buttons).
-- Items should use this functions instead of trying to access the popup layer
-- directly.
popupItem :: Monad m => Glance m l (Window m) (Component m)
popupItem w = 
    let insidepop = children >=> contentsPane >=> descendants >=> \c -> 
            case cType c of
                PopupMenu -> descendants c
                _ -> mzero
    in (popupLayer >=> descendants $ w) `mplus` 
       (insidepop >=> return . Component . lower . unComponentW $ w)

-- | Performs a sequence of selections in a window menu, based to the text of
-- the options. Pass it something like 
--
-- > map (==) ["menuitem1","menuitem2',...]
--
-- To match the exact names of the options.
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

-- | Like 'selectInMenuBar', but for when the last item is a toggleable component. The bool paramenter is the desired selection state.
toggleInMenuBar :: Monad m => Bool -> [T.Text -> Bool] -> Pianola m l (Window m) ()
toggleInMenuBar toggleStatus ps = 
    let go (firstitem,middleitems,lastitem) = do
           poke $ replusify._menu.wInfo >=> descendants >=> hasText firstitem >=> clickButton
           let pairs = zip middleitems (clickButton <$ middleitems) ++
                       [(lastitem, toggle toggleStatus)]
           forM_ pairs $ \(txt,action) -> 
               pmaybe pfail $ retryPoke1s 7 $ 
                   popupItem >=> hasText txt >=> action
           replicateM_ (length pairs) $ poke escape
        clip l = (,,) <$> headZ l <*> (initZ l >>= tailZ)  <*> lastZ l
    in maybe pfail go (clip ps)

-- | Clicks on a combo box component and selects an option by its text. mzero if the component is not a combo box.
selectInComboBox :: (Monad m, ComponentLike c, Windowed c) => (T.Text -> Bool) -> Pianola m l (c m) ()
selectInComboBox f = do
        poke $ clickCombo
        poke $ window >=> popupItem >=> listCellByText f >=> return._clickCell

-- | If the component is a tabbed pane returns the select action of a tab based on its text. 'mzero' if the component is not a tabbed pane.
selectTabByText :: (Monad m,ComponentLike c) => (T.Text -> Bool) -> Glance m l (c m) (Sealed m)
selectTabByText f =  
    tab >=> \aTab -> do    
        guard $ f . _tabText $ aTab
        return $ _selectTab aTab   

selectTabByToolTip :: (Monad m,ComponentLike c) => (T.Text -> Bool) -> Glance m l (c m) (Sealed m)
selectTabByToolTip f =  
    tab >=> \aTab -> do    
        tooltip <- justZ . _tabToolTip $ aTab
        guard $ f tooltip
        return $ _selectTab aTab   

-- | Returns the expand action of the root node of a tree of cells. Useful with trees.
expand :: Monad m => Bool -> Glance m l (Tree (Cell m)) (Sealed m)
expand b cell = (justZ . _expand . rootLabel $ cell) <*> pure b

-- | Takes a component, finds a label by text in the descendants of the component, finds the component labeled by it (by position) and returns the later. 
--
-- Useful for targeting text fields in form-like dialogs.
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



