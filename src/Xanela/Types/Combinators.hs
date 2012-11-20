{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Xanela.Types.Combinators (
        MK,
        LK,
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

type MK it ot m = it m -> MaybeT (LogProducer m) (ot m)
type LK it ot m = it m -> LogicT (LogProducer m) (ot m)

retry:: (Monad m,MonadBase m m) => Int -> [MK GUI GUI m] -> MK GUI GUI m
retry delaytime [] gui = mzero
retry delaytime (guik:l) gui = MaybeT $ do
        result <- runMaybeT . guik $ gui
        case result of 
            Just j -> return $ Just j 
            Nothing -> runMaybeT ( wait 1 gui >>= retry delaytime l )

withMenuBar::(Monad m, MonadBase m m) => LK GUI WindowInfo m -> Maybe Bool -> [T.Text -> Bool] -> MK GUI GUI m
withMenuBar winlocator actionType ps = 
    let lastItemAction = maybe click toggle actionType
        escapes n = composeK . replicate n . narrowK $ winlocator >=> escape
        withMenuBar' firstitem middleitems lastitem =  
            let retryfyK = retry 1 . (:) (const mzero) . replicate 7
            in narrowK ( winlocator >=> menuflat >=> text firstitem >=> click ) >=>
               threadKs pure (retryfyK.narrowK) (winlocator >=> popupflat,click) (map text middleitems) >=>
               (retryfyK.narrowK $ winlocator >=> popupflat >=> text lastitem >=> lastItemAction ) >=>
               maybe pure (const . escapes . succ . length $ middleitems ) actionType
    in case ps of 
        p':ps' ->  case reverse ps' of
            p'':ps'' -> withMenuBar' p' (reverse ps'') p''
            _ -> const mzero    
        _ -> const mzero

withMenuBarEq winlocator actionType ts = withMenuBar winlocator actionType (map (==) ts) 

