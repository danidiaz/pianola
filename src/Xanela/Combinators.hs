{-# LANGUAGE TemplateHaskell,GeneralizedNewtypeDeriving,OverloadedStrings #-}

module Xanela.Combinators (
        clickButtonWithText
    ) where

import Prelude hiding (catch,(.))
import System.IO
import System.Environment
import System.Console.GetOpt
import Data.Char
import qualified Data.Map as M
import Data.List
import Data.Lens.Common
import Data.Lens.Template
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
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Applicative
import Control.Exception
import Network
import Blaze.ByteString.Builder
import Data.MessagePack
import Data.MessagePack.Object
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Xanela
import Control.Monad.Logic
import Debug.Trace (trace)

l2l :: MonadPlus m => [a] -> m a
l2l = msum . map return

clickButtonWithText:: [Window] -> Xanela ()
clickButtonWithText wl = do
    let button::Logic (Xanela ())
        button = do
            w <- l2l wl
            c <- l2l . map _topc . flatten $ w
            ci <- l2l . flatten $ c  
            Just txt <- return . _text $ ci 
            guard $ txt == "foo"
            Button xa <- return . _componentType $ ci   
            return xa
    observe button


