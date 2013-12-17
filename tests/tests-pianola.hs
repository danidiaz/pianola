{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import Prelude hiding (catch,(.),id)
import Data.Monoid
import Data.Tree
import qualified Data.Text as T
import Network 
import Control.Lens
import Control.Category
import Control.Arrow
import Control.Monad
import Control.Monad.Error
import Control.Applicative
import Control.Comonad.Trans.Class

import Pianola
import Pianola.Util
import Pianola.Player
import Pianola.Swing
import Pianola.Swing.Protocol (snapshot,remote)

import System.Environment
import System.Exit (exitFailure)

main :: IO ()
main = do
  putStrLn "foo"
