{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Pianola (
        ObserverF(..),
        liftNp,
        Observer(..),
        focusO,
        Pianola(..),
        Delay,
        play,
        oops,
        squint,
        peek,
--        peekl,
        poke,
--        pokel,
        retryPeek,
        retryPoke,
        sleep,
        focus --,  
--        focusl
    ) where

import Prelude hiding (catch,(.))
import System.IO
import System.Environment
import System.Console.GetOpt
import Data.Tree
import Data.Functor.Compose
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

type Glance o l m a = o -> MaybeT (Pr l (Nullipotent m)) a

type Wideglance o l m a = o -> LogicT (Pr l (Nullipotent m)) a

squint :: Monad m => (a -> LogicT m b) -> a -> MaybeT m b 
squint = fmap tomaybet

--
--

liftNp :: (Monad m, MonadTrans mt) => Nullipotent m a -> mt (Pr l (Nullipotent m)) a
liftNp = lift . lift

type ObserverF o l m = Compose ((->) o) (MaybeT (Pr l (Nullipotent m)))

type Observer o l m = Free (ObserverF o l m)

focusO :: (Functor m, Monad m) => Glance o' l m o -> Observer o l m a -> Observer o' l m a
focusO prefix v =
   let nattrans (Compose k) = Compose $ prefix >=> k
   in hoistFree nattrans v

playO :: Monad m => m o -> Observer o l m a -> MaybeT (Pr l m) a
playO _ (Pure b) = return b
playO mom (Free f) =
   let removeNullipotent = fmap.mapMaybeT $ hoist runNullipotent
   in join $ (lift . lift $ mom) >>= removeNullipotent (getCompose $ playO mom <$> f)

type Delay = Int

type Pianola o l m = Pr (Sealed m) (Pr Delay (MaybeT (Pr l (Observer o l m))))  

play :: Monad m => m o -> Pianola o l m a -> Pr Delay (MaybeT (Pr l (MaybeT (Pr l m)))) a
play mom pi =
    let pianola' = hoist (hoist (mapMaybeT (hoist $ playO mom))) $ pi 
        injector () = forever $ do
            s <- request ()
            lift . lift . lift . lift . lift . lift $ unseal s -- this should be private
    in runProxy $ const pianola' >-> injector


oops :: Monad m => Pianola o l m a
oops = lift . lift $ mzero

peek :: Monad m => Glance o l m a -> Pianola o l m a
peek = lift . lift . lift . lift . liftF . Compose

--peekl :: Monad m => Multiglance o l m a -> Pianola o l m a
--peekl = peek.narrowK

poke :: Monad m => Glance o l m (Sealed m) -> Pianola o l m () 
poke locator = peek locator >>= respond

--pokel :: Monad m => Multiglance o l m (Sealed m) -> Pianola o l m () 
--pokel = poke.narrowK

retryPeek :: Monad m => Delay -> [Glance o l m a] -> Pianola o l m a 
retryPeek _ [] = oops
retryPeek d (x:xs) = do
    a <- peek . fmap (lift . runMaybeT) $ x
    maybe (sleep d >> retryPeek d xs) return a

retryPoke :: Monad m => Delay -> [Glance o l m (Sealed m)] -> Pianola o l m () 
retryPoke d xs = retryPeek d xs >>= respond 

sleep :: Monad m => Delay -> Pianola o l m ()
sleep = lift . respond 

focus :: (Functor m, Monad m) => Glance o' l m o ->  Pianola o l m a -> Pianola o' l m a 
focus prefix pi  =
    hoist (hoist (mapMaybeT (hoist $ focusO prefix))) $ pi 

--focusl :: (Functor m, Monad m) => Multiglance o' l m o ->  Pianola o l m a -> Pianola o' l m a 
--focusl g = focus (narrowK g)

infixl 0 `focus`

--
instance Monad m => PianolaLog (Pianola o LogEntry m) where
    xanlog = lift . lift . lift . xanlog
