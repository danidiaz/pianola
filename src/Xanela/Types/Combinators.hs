{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Xanela.Types.Combinators (
        retry,
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
import Control.Monad.Trans.Class
import Control.Monad.Logic
import Control.Monad.Trans.Maybe

import Xanela.Util
import Xanela.Types


retry:: (MonadBase n m) => Int -> [GUI n -> MaybeT m (GUI n)] -> GUI n -> MaybeT m (GUI n)
retry delaytime [] gui = mzero
retry delaytime (guik:l) gui = MaybeT $ do
        result <- runMaybeT . guik $ gui
        case result of 
            Just j -> return $ Just j 
            Nothing -> runMaybeT ( wait 1 gui >>= retry delaytime l )

withMenuBar::(MonadBase n m) => (GUI n -> LogicT m (WindowInfo n)) -> 
                                Maybe Bool -> 
                                [T.Text -> Bool] ->
                                GUI n -> MaybeT m (GUI n) 
withMenuBar winlocator actionType ps = 
    let lastItemAction = maybe click toggle actionType
        escapes n = composeK . replicate n . narrowK $ winlocator >=> escape
        withMenuBar' firstitem middleitems lastitem =  
            let retryfyK = retry 1 . (:) (const mzero) . replicate 7
                middleactions = map text middleitems
            in narrowK ( winlocator >=> menuflat >=> text firstitem >=> click ) >=>
               threadKs return (retryfyK.narrowK) (winlocator >=> popupflat) click middleactions >=>
               (retryfyK.narrowK $ winlocator >=> popupflat >=> text lastitem >=> lastItemAction ) >=>
               maybe return (const . escapes . succ . length $ middleitems ) actionType
    in case ps of 
        p':ps' ->  case reverse ps' of
            p'':ps'' -> withMenuBar' p' (reverse ps'') p''
            _ -> const mzero    
        _ -> const mzero

withMenuBarEq winlocator actionType ts = withMenuBar winlocator actionType (map (==) ts) 

