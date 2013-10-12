{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Pianola.Swing (
        module Pianola.Swing.Internal,
        clickButtonByText,
        rightClickByText,
        popupItem,
        selectInMenuBar,
        toggleInMenuBar,
        selectInComboBox,
        selectTabByText, 
        tableCellByText,
        labeledBy,
        logcapture
    ) where

import Prelude hiding (catch)
import Data.Tree
import Data.Function
import Data.Functor.Identity
import qualified Data.Text as T
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
import Safe
import Pianola
import Pianola.Util
import Pianola.Geometry
import Pianola.Swing.Internal

instance Geometrical ComponentInfo where
    nwcorner = _pos
    dimensions = _dim

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

clip :: [a] -> Maybe (a,[a],a)
clip l = (,,) <$> headMay l <*> (initMay >=> tailMay $ l) <*> lastMay l

menuBarOp :: Monad m => Selector m l GUIComponent (Change m) 
                     -> Selector m l GUIComponent (Change m) 
                     -> (T.Text->Bool, [T.Text -> Bool], T.Text->Bool ) 
                     -> Pianola m l GUIWindow ()
menuBarOp middleAction lastAction (firstitem,middleitems,lastitem) = do
       poke $ decorate (the.menu.folded) >>> 
              descendants >>> 
              prune (the.text._Just) firstitem >>> 
              middleAction 
       let pairs = zip middleitems (repeat middleAction) ++ [(lastitem, lastAction)]
       forM_ pairs $ \(txt,action) -> 
           throwIfZero "Menu select: can't find item." $ retryPoke1s 7 $ 
               popupItem >>> prune (the.text._Just) txt >>> action

selectInMenuBar :: Monad m => Remote m -> [T.Text -> Bool] -> Pianola m l GUIWindow ()
selectInMenuBar remote ps = case clip ps of 
          Nothing -> throwError "Menu select: top level option and at least one suboption required."  
          Just items -> menuBarOp (clickButton remote) (clickButton remote) items

toggleInMenuBar :: Monad m => Remote m -> Bool -> [T.Text -> Bool] -> Pianola m l GUIWindow ()
toggleInMenuBar remote toggleStatus ps = case clip ps of 
        Nothing -> throwError "Menu select: top level option and at least one suboption required."
        Just items@(_,b,_) -> do menuBarOp (clickButton remote) (toggle remote toggleStatus) items
                                 replicateM_ (length b + 1) $ poke $ escape remote

logcapture :: Monad m => Remote m -> T.Text -> Pianola m LogEntry GUIWindow ()
logcapture r caption = (peek $ arr (capture r) >>> liftQ) >>= logimg caption

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
        labelArrow = descendants >>>
                     prune (the.componentType._Label) (\_->True) >>> 
                     prune (the.text._Just) f    
        candidatesArrow = descendants >>> 
                      prune (the.componentType) labellable 
        candidates = collect $ 
            (labelArrow &&& candidatesArrow) >>> prune id (uncurry sameLevelRightOf) >>^ snd 
     in candidates >>> Kleisli (replusify . headMay . sortBy (compare `on` minX))

