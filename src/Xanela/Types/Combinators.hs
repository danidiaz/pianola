{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Xanela.Types.Combinators (
        withMenuBar,
        withMenuBarEq
    ) where

import Prelude hiding (catch,(.))
import Data.Tree
import qualified Data.Text as T
import Control.Category
import Control.Error
import Control.Monad
import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Logic
import Control.Monad.Trans.Maybe

import Xanela.Util
import Xanela.Types

withMenuBar::(MonadBase n m) => (GUI n -> LogicT m (WindowInfo n)) -> 
                                (GUI n -> LogicT m (GUI n)) -> 
                                [T.Text -> Bool] ->
                                GUI n -> MaybeT m (GUI n) 
withMenuBar winlocator callback [] g = mzero
withMenuBar winlocator callback ( p:ps ) g = do
    g <- maybeify $ winlocator >=> menuflat >=> text p >=> click $ g
    sandwich (winlocator >=> popupflat) (click >=> callback) (map text ps) $ g
 
withMenuBarEq winlocator callback ts = withMenuBar winlocator callback (map (==) ts)
     


   

