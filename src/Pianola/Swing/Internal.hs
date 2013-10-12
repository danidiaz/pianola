{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Pianola.Swing.Internal where

import Prelude hiding (catch)
import Data.Tree
import Data.Function
import Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.ByteString as B
import Control.Lens
import Control.Arrow
import Control.Monad
import Control.Arrow
import Control.Comonad
import Control.Applicative
import Control.Monad.Error
import Control.Monad.Trans.Class
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env    
import Data.List
import Data.Functor.Identity
import Control.Monad.Logic
import Data.Aeson
import Data.Aeson.TH

import Pianola.Internal (Query,Change)
import Pianola.Orphans

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

$(makeLenses ''GUI)
$(makeLenses ''WindowInfo)
$(makeLenses ''ComponentInfo)
$(makePrisms ''ComponentType)
$(makeLenses ''CellInfo)
$(makeLenses ''TabInfo)

$(deriveJSON defaultOptions ''GUI)
$(deriveJSON defaultOptions ''WindowInfo)
$(deriveJSON defaultOptions ''ComponentInfo)
$(deriveJSON defaultOptions ''ComponentType)
$(deriveJSON defaultOptions ''TabInfo)
$(deriveJSON defaultOptions ''CellInfo)

data Remote m = Remote
    { toFront ::     MonadPlus n => Kleisli n GUIWindow (Change m)
    , escape::       MonadPlus n => Kleisli n GUIWindow (Change m)
    , enter::        MonadPlus n => Kleisli n GUIWindow (Change m)
    , close::        MonadPlus n => Kleisli n GUIWindow (Change m)
    , capture ::                    GUIWindow -> 
                                    Query m B.ByteString
    
    , click ::       MonadPlus n => Kleisli n GUIComponent (Change m)
    , doubleClick :: MonadPlus n => Kleisli n GUIComponent (Change m)
    , rightClick ::  MonadPlus n => Kleisli n GUIComponent (Change m)
    , toggle::       MonadPlus n => Bool -> 
                                    Kleisli n GUIComponent (Change m)
    , clickButton :: MonadPlus n => Kleisli n GUIComponent (Change m)
    , clickCombo::   MonadPlus n => Kleisli n GUIComponent (Change m)
    , setText ::     MonadPlus n => T.Text -> 
                                    Kleisli n GUIComponent (Change m)
    , selectTab::    MonadPlus n => Kleisli n GUITab (Change m)

    , clickCell ::       (Comonad c, MonadPlus n) => Kleisli n (EnvT GUIComponent c CellInfo) (Change m)
    , doubleClickCell :: (Comonad c, MonadPlus n) => Kleisli n (EnvT GUIComponent c CellInfo) (Change m)
    , rightClickCell ::  (Comonad c, MonadPlus n) => Kleisli n (EnvT GUIComponent c CellInfo) (Change m)
    , expand ::          (Comonad c, MonadPlus n) => Bool -> 
                                                     Kleisli n (EnvT GUIComponent c CellInfo) (Change m)
    }

