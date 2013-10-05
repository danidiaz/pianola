{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Pianola.Swing (
        GUI (..), snapshotId, topLevel,
        WindowInfo (..), windowId,windowTitle,windowDim,menu,popupLayer,contentPane,
        Window,
        GUIWindow,
        ComponentInfo (..), componentId,pos,dim,name,tooltip,text,enabled,componentType,
        ComponentType (..), _Panel,_Toggleable,_Button,_TextField
                          , _Label,_ComboBox
                          , _List, _Table, _Treegui,_PopupMenu,_TabbedPane,_Other, 
        Component,
        GUIComponent,
        CellInfo (..), rowId,columnId,isFromTree,renderer,
        ListCell,
        TableCell,
        TreeCell,
        TabInfo (..), tabId,tabText,tabToolTip,isTabSelected,
        Tab,
        GUITab,
        clickButtonByText,
        rightClickByText,
        popupItem,
        selectInMenuBar,
        toggleInMenuBar,
        selectInComboBox,
        selectTabByText, 
        tableCellByText,
        labeledBy,
        logcapture,
        Remote(..)
    ) where

import Prelude hiding (catch)
import Data.Tree
import Data.Function
import Data.Functor.Identity
import qualified Data.Text as T
import Control.Lens
import Control.Arrow
import Control.Error
import Control.Monad
import Control.Arrow
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
import Safe


data GUI = GUI { _snapshotId :: Int
               , _topLevel :: [Window] 
               }

data WindowInfo = WindowInfo 
    {  _windowId::Int
    ,  _windowTitle::T.Text
    ,  _windowDim::(Int,Int) 
    ,  _menu::[Component]
    ,  _popupLayer:: [Component]
    ,  _contentPane::Component
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
    } 


instance Geometrical ComponentInfo where
    nwcorner = _pos
    dimensions = _dim

type Component = Tree ComponentInfo

type GUIComponent = EnvT GUIWindow Tree ComponentInfo

data ComponentType =
     Panel
    |Toggleable Bool 
    |Button 
    |TextField Bool 
    |Label
    |ComboBox (Maybe Component) 
    |List [ListCell]
    |Table [[TableCell]]
    |Treegui [TreeCell] 
    |PopupMenu  
    |TabbedPane [Tab]
    |Other T.Text

data CellInfo = CellInfo 
    { _rowId::Int
    , _columnId::Int
    , _renderer::Component 
    , _isFromTree::Bool
    }

type ListCell = Identity CellInfo

type TableCell = Identity CellInfo

type TreeCell = Tree CellInfo

data TabInfo = TabInfo
    { _tabId::Int 
    , _tabText::T.Text
    , _tabToolTip::Maybe T.Text
    , _isTabSelected:: Bool
    }

type Tab = Identity TabInfo
type GUITab = EnvT GUIComponent Identity TabInfo

makeLenses ''GUI
makeLenses ''WindowInfo
makeLenses ''ComponentInfo
makePrisms ''ComponentType
makeLenses ''CellInfo
makeLenses ''TabInfo

data Remote m = Remote
    { clickButton :: MonadPlus n => Kleisli n GUIComponent (Change m)
    , toFront ::     MonadPlus n => Kleisli n GUIWindow    (Change m)
    , setText ::     MonadPlus n => T.Text -> 
                                    Kleisli n GUIComponent (Change m)
    , click ::       MonadPlus n => Kleisli n GUIComponent (Change m)
    , doubleClick :: MonadPlus n => Kleisli n GUIComponent (Change m)
    , rightClick ::  MonadPlus n => Kleisli n GUIComponent (Change m)
    , toggle::       MonadPlus n => Bool -> 
                                    Kleisli n GUIComponent (Change m)
    , clickCombo::   MonadPlus n => Kleisli n GUIComponent (Change m)
    , selectTab::    MonadPlus n => Kleisli n GUITab (Change m)
    , clickCell ::       (Comonad c, MonadPlus n) => Kleisli n (EnvT GUIComponent c CellInfo) (Change m)
    , doubleClickCell :: (Comonad c, MonadPlus n) => Kleisli n (EnvT GUIComponent c CellInfo) (Change m)
    , rightClickCell ::  (Comonad c, MonadPlus n) => Kleisli n (EnvT GUIComponent c CellInfo) (Change m)
    , expand ::          (Comonad c, MonadPlus n) => Bool -> 
                                                     Kleisli n (EnvT GUIComponent c CellInfo) (Change m)
    , escape::       MonadPlus n => Kleisli n GUIWindow (Change m)
    , enter::        MonadPlus n => Kleisli n GUIWindow (Change m)
    , close::        MonadPlus n => Kleisli n GUIWindow (Change m)
    , capture :: GUIWindow -> Query m Image 
    }

clickButtonByText :: Monad m => Remote m -> (T.Text -> Bool) -> Selector m l GUIComponent (Change m) 
clickButtonByText p predicate = descendants >>> prune (the.text._Just) predicate >>> clickButton p

rightClickByText :: Monad m => Remote m -> (T.Text -> Bool) -> Selector m l GUIComponent (Change m) 
rightClickByText p predicate = descendants >>> prune (the.text._Just) predicate >>> rightClick p

popupItem :: Monad m => Selector m l GUIWindow GUIComponent
popupItem = (decorate (the.popupLayer.folded) >>> descendants) <+> insidepop
    where insidepop = descendants1 >>> 
                      decorate (the.contentPane) >>> 
                      descendants >>> 
                      prune (the.componentType._PopupMenu) (const True) >>> 
                      descendants

selectInMenuBar :: Monad m => Remote m -> [T.Text -> Bool] -> Pianola m l GUIWindow ()
selectInMenuBar r ps = 
    let go (firstitem,middleitems,lastitem) = do
           poke $ decorate (the.menu.folded) >>> descendants >>> prune (the.text._Just) firstitem >>> clickButton r
           let pairs = zip middleitems (clickButton r <$ middleitems) ++
                       [(lastitem, clickButton r)]
           forM_ pairs $ \(txt,action) -> 
               pmaybe pfail $ retryPoke1s 7 $ 
                   popupItem >>> prune (the.text._Just) txt >>> action
        clip l = (,,) <$> headZ l <*> (initZ l >>= tailZ) <*> lastZ l
    in maybe pfail go (clip ps)

toggleInMenuBar :: Monad m => Remote m -> Bool -> [T.Text -> Bool] -> Pianola m l GUIWindow ()
toggleInMenuBar r toggleStatus ps = 
    let go (firstitem,middleitems,lastitem) = do
           poke $ decorate (the.menu.folded) >>> descendants >>> prune (the.text._Just) firstitem >>> clickButton r
           let pairs = zip middleitems (clickButton r <$ middleitems) ++
                       [(lastitem, toggle r toggleStatus)]
           forM_ pairs $ \(txt,action) -> 
               pmaybe pfail $ retryPoke1s 7 $ 
                   popupItem >>> prune (the.text._Just) txt >>> action
           replicateM_ (length pairs) $ poke $ escape r
        clip l = (,,) <$> headZ l <*> (initZ l >>= tailZ)  <*> lastZ l
    in maybe pfail go (clip ps)

logcapture :: Monad m => Remote m -> Pianola m LogEntry GUIWindow ()
logcapture r = (peek $ arr (capture r) >>> liftQ) >>= logimg

selectInComboBox :: Monad m => Remote m -> (T.Text -> Bool) -> Pianola m l GUIComponent ()
selectInComboBox r f = do
        poke $ clickCombo r
        poke $ context >>> 
               popupItem >>> 
               decorate (the.componentType._List.folded) >>> 
               prune (the.renderer.folded.text._Just) f >>> 
               clickCell r

selectTabByText :: Monad m => Remote m -> (T.Text -> Bool) -> Selector m l GUIComponent (Change m)
selectTabByText r f = decorate (the.componentType._TabbedPane.folded) >>> prune (the.tabText) f >>> selectTab r  

tableCellByText:: MonadPlus n => Int -> (T.Text -> Bool) -> Kleisli n GUIComponent (EnvT GUIComponent Identity CellInfo)
tableCellByText colIndex f =
    decorate (the.componentType._Table.folding (`atMay` colIndex).folded) >>>    
    prune (the.renderer.folded.text._Just) f

labeledBy :: Monad m => (T.Text -> Bool) -> Selector m l GUIComponent GUIComponent
labeledBy f = 
    let labellable c = case c of
            Toggleable {} -> True
            Button {} -> True
            TextField {} -> True
            ComboBox {} -> True
            List {} -> True
            Table {} -> True
            Treegui {} -> True
            _ -> False
        firstArrow = descendants >>>
                     prune (the.componentType._Label) (\_->True) >>> 
                     prune (the.text._Just) f    
        secondArrow = descendants >>> 
                      prune (the.componentType) labellable 
        candidates = collect $ 
            (firstArrow &&& secondArrow) >>> prune id (uncurry sameLevelRightOf) >>^ snd 
     in candidates >>> Kleisli (headZ . sortBy (compare `on` minX))

