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
                                Maybe Bool -> 
                                [T.Text -> Bool] ->
                                GUI n -> MaybeT m (GUI n) 
withMenuBar winlocator actionType ps = 
    let lastItemAction = maybe click toggle actionType
        withMenuBar' firstitem middleitems lastitem =  
            maybeifyK ( winlocator >=> menuflat >=> text firstitem >=> click ) >=>
            maybeifyManyK (winlocator >=> popupflat) click (map text middleitems) >=>
            maybeifyK ( winlocator >=> popupflat >=> text lastitem >=> lastItemAction)
    in case ps of 
        p':ps' ->  case reverse ps' of
            p'':ps'' -> withMenuBar' p' (reverse ps'') p''
            _ -> const mzero    
        _ -> const mzero

withMenuBarEq winlocator actionType ts = withMenuBar winlocator actionType (map (==) ts) 

