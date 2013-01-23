{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Pianola (
        ObserverF(..),
        Observer(..),
        focusObserver,
        Pianola(..),
        Delay,
        playPianola,
        peek,
        poke,
        keepPoking,
        sleep,
        focus   
    ) where

import Prelude hiding (catch,(.))
import System.IO
import System.Environment
import System.Console.GetOpt
import Data.Tree
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network 
import Control.Category
import Control.Error
import Control.Applicative
import Control.Proxy
import Control.MFunctor
import Control.Monad
import Control.Comonad
import Control.Exception
import Control.Monad.Base
import Control.Monad.Free
import Control.Monad.Trans.Reader
import Control.Monad.Logic
import Control.Monad.Trans.Maybe
--import Control.Monad.Trans.Free  

import Pianola.Util

type Pr t = Producer ProxyFast t

type Observation l m a = MaybeT (Pr l (Nullipotent m)) a 

data ObserverF l o a = ObserverF { runObserverF :: forall m. Monad m => o m -> Observation l m a }

instance Functor (ObserverF l o) where
   fmap f (ObserverF x) = ObserverF $ (fmap.liftM) f x

type Observer l o = Free (ObserverF l o)

focusObserver :: (forall m. Monad m => o m -> Observation l m (o' m)) -> Observer l o' a -> Observer l o a
focusObserver prefix v =
   let nattrans (ObserverF k) = ObserverF $ prefix >=> k
   in hoistFree nattrans v

runObserver :: Monad m => m (o m) -> Observer l o a -> MaybeT (Pr l m) a
runObserver _ (Pure b) = return b
runObserver mom (Free f) =
   let removeNullipotent = fmap.mapMaybeT $ hoist runNullipotent
   in join $ (lift . lift $ mom) >>= removeNullipotent (runObserverF $ runObserver mom <$> f)

type Delay = Int

type Pianola l l' o m a = Pr (Sealed m) (Pr Delay (MaybeT (Pr l (Observer l' o)))) a 

playPianola :: Monad m => m (o m) -> Pianola l l' o m a -> Pr Delay (MaybeT (Pr l (MaybeT (Pr l' m)))) a
playPianola mom pi =
    let pianola' = hoist (hoist (mapMaybeT (hoist $ runObserver mom))) $ pi 
        injector () = forever $ do
            s <- request ()
            lift . lift . lift . lift . lift . lift $ unseal s -- this should be private
    in runProxy $ const pianola' >-> injector

--
--
peek :: (forall m. Monad m => o m -> Observation l m a) -> Pianola l' l o m a
peek = lift . lift . lift . lift . liftF . ObserverF  

poke :: (forall m. Monad m => o m -> Observation l m (Sealed m)) -> Pianola l' l o m () 
poke locator = peek locator >>= respond

keepPoking :: Int -> Delay -> (forall m. Monad m => o m -> Observation l m (Maybe (Sealed m))) -> Pianola l' l o m () 
keepPoking = undefined

sleep :: Delay -> Pianola l l' o m ()
sleep = lift 

focus :: (forall m. Monad m => o m -> Observation l' m (o' m)) ->  Pianola l l' o m a -> Pianola l l' o' m a 
focus prefix pi  =
    hoist (hoist (mapMaybeT (hoist $ focusObserver prefix))) $ pi 


