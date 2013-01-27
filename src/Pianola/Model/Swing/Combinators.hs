{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Pianola.Model.Swing.Combinators (
        withMenuBar
    ) where

import Prelude hiding (catch,(.))
import Data.Tree
import Data.Sequence (ViewL(..),ViewR(..),viewl,viewr,fromList)
import Data.Foldable (toList)
import qualified Data.Text as T
import Control.Category
import Control.Error
import Control.Monad
import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Class
import Control.Monad.Logic
import Control.Monad.Trans.Maybe

import Pianola
import Pianola.Util
import Pianola.Model.Swing

withMenuBar:: Monad m => [T.Text -> Bool] -> Maybe Bool -> Pianola (WindowInfo m) l m ()
withMenuBar ps liatype@(maybe click toggle -> lastItemAction) = 
      let go firstitem middleitems lastitem = do
             poke.lo $ menuflat >=> text firstitem >=> click
             forM_ middleitems $ \f -> 
                retryPoke 1 $ replicate 7 $ lo $ popupflat >=> text f >=> click
             retryPoke 1 $ replicate 7 $ lo $ popupflat >=> text lastitem >=> lastItemAction
             when (isJust liatype) $ replicateM_ (succ $ length middleitems) (poke $ return . _escape)
      in case (viewl . fromList $ ps) of 
          firstitem :< ps' ->  case viewr ps' of
              ps'' :> lastitem -> go firstitem (toList ps'') lastitem
              _ -> oops
          _ -> oops
  
