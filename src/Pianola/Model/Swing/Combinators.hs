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
             poke_ $ menuflat >=> text firstitem >=> click
             forM_ middleitems $ \f -> 
                retryPoke_ 1 $ replicate 7 $ popupflat >=> text f >=> return . click
             retryPoke_ 1 $ replicate 7 $ popupflat >=> text lastitem >=> return . lastItemAction
             when (isJust liatype) $ replicateM_ (succ $ length middleitems) (poke $ return . _escape)
      in case (viewl . fromList $ ps) of 
          firstitem :< ps' ->  case viewr ps' of
              ps'' :> lastitem -> go firstitem (toList ps'') lastitem
              _ -> oops
          _ -> oops
  
--withMenuBar winlocator actionType ps = 
--    let lastItemAction = maybe click toggle actionType
--        escapes n = composeK . replicate n . narrowK $ winlocator >=> escape
--        withMenuBar' firstitem middleitems lastitem =  
--            let retryfyK = retry 1 . (:) (const mzero) . replicate 7
--            in narrowK ( winlocator >=> menuflat >=> text firstitem >=> click ) >=>
--               threadKs pure (retryfyK.narrowK) (winlocator >=> popupflat,click) (map text middleitems) >=>
--               (retryfyK.narrowK $ winlocator >=> popupflat >=> text lastitem >=> lastItemAction ) >=>
--               maybe pure (const . escapes . succ . length $ middleitems ) actionType
--    in case ps of 
--        p':ps' ->  case reverse ps' of
--            p'':ps'' -> withMenuBar' p' (reverse ps'') p''
--            _ -> const mzero    
--        _ -> const mzero
--

