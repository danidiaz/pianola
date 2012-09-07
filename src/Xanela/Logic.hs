{-# LANGUAGE TemplateHaskell,ScopedTypeVariables,GeneralizedNewtypeDeriving,FlexibleInstances #-}

module Xanela.Logic (
        window,
        wait4changes,
        contents,
        popup,
        menu,
        text,
        click,
        toggle,
        rightClick,
        setText               
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
import Control.Monad.Logic
import Control.Monad.Trans
import Xanela.Types

import Debug.Trace (trace)

-- logic helpers
mplusify :: MonadPlus m => [a] -> m a
mplusify = msum . map return

-- search combinators
window :: GUI m -> LogicT m (WindowInfo m)
window gui = do
    w <- mplusify . _gui $ gui
    mplusify $ flatten w

wait4changes :: (Monad m) => Int -> GUI m -> LogicT m (GUI m)
wait4changes d gui = lift $ _wait4changes gui d

contents :: WindowInfo m -> LogicT m (ComponentInfo m)
contents = mplusify . flatten . _topc
    
popup :: WindowInfo m -> LogicT m (ComponentInfo m)
popup w = do 
    c <- mplusify . _popupLayer $ w
    mplusify . flatten $ c

menu :: WindowInfo m -> LogicT m (ComponentInfo m)
menu w = do
    c <- mplusify . _menu $ w
    mplusify . flatten $ c

text:: T.Text -> ComponentInfo m -> LogicT m (ComponentInfo m)
text desiredtxt c = case _text c of 
    Just txt -> do guard $ txt == desiredtxt 
                   return c
    _ -> mzero

click:: (Monad m) => ComponentInfo m -> LogicT m (GUI m)
click c = case _componentType c of
    Button _ xa -> lift xa
    _ -> mzero

toggle:: (Monad m) => Bool -> ComponentInfo m -> LogicT m (GUI m)
toggle state c = case _componentType c of
    Button (Just state) xa -> lift xa
    _ -> mzero

rightClick:: (Monad m) => ComponentInfo m -> LogicT m (GUI m)
rightClick = lift . _rightClick

setText:: (Monad m) => T.Text -> ComponentInfo m -> LogicT m (GUI m)
setText txt c = case _componentType c of
    TextField (Just f) -> lift $ f txt
    _ -> mzero

