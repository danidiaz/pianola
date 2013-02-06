{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Pianola (
        Glance(..),
        notPresent,
        collect,
        anyOf,
        children,
        descendants,
        liftN,
        ObserverF(..),
        liftN,
        Pianola(..),
        Delay,
        play,
        pianolaProblem,
        peek,
        poke,
        retryPeek,
        retryPoke,
        sleep,
        with
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

import Pianola.Util

type Glance m l o a = o -> LogicT (Prod l (Nullipotent m)) a

notPresent :: Monad m => Glance m l o a -> Glance m l o () 
notPresent = fmap lnot

collect :: Monad m => Glance m l o a -> Glance m l o [a] 
collect = fmap $ lift . observeAllT

anyOf :: Glance m l [o] o 
anyOf = replusify

children :: Glance m l (Tree o) (Tree o)
children = replusify . subForest 

descendants :: Glance m l (Tree o) (Tree o)
descendants = replusify . flatten . duplicate

liftN :: Monad m => Glance m l (Nullipotent m a) a
liftN = lift . lift

type ObserverF m l o = Compose ((->) o) (LogicT (Prod l (Nullipotent m)))

type Observer m l o = Free (ObserverF m l o)

focus :: (Functor m, Monad m) => Glance m l o' o -> Observer m l o a -> Observer m l o' a
focus prefix v =
   let nattrans (Compose k) = Compose $ prefix >=> k
   in hoistFree nattrans v

runObserver :: Monad m => m o -> Observer m l o a -> MaybeT (Prod l m) a
runObserver _ (Pure b) = return b
runObserver mom (Free f) =
   let squint = fmap $ mapMaybeT (hoist runNullipotent) . tomaybet
   in join $ (lift . lift $ mom) >>= squint (getCompose $ runObserver mom <$> f)

type Delay = Int

type Pianola m l o = Prod (Sealed m) (Prod Delay (MaybeT (Prod l (Observer m l o))))  

play :: Monad m => m o -> Pianola m l o a -> Prod Delay (MaybeT (Prod l (MaybeT (Prod l m)))) a
play mom pi =
    let pianola' = hoist (hoist (mapMaybeT (hoist $ runObserver mom))) $ pi 
        injector () = forever $ do
            s <- request ()
            lift . lift . lift . lift . lift . lift $ unseal s -- unseal should be private
    in runProxy $ const pianola' >-> injector

pianolaProblem :: Monad m => Pianola m l o a
pianolaProblem = lift . lift $ mzero

peek :: Monad m => Glance m l o a -> Pianola m l o a
peek = lift . lift . lift . lift . liftF . Compose

poke :: Monad m => Glance m l o (Sealed m) -> Pianola m l o () 
poke locator = peek locator >>= respond

retryPeek :: Monad m => Delay -> [Glance m l o a] -> Pianola m l o a 
retryPeek _ [] = pianolaProblem
retryPeek d (x:xs) = do
    a <- peek . fmap maybify $ x
    maybe (sleep d >> retryPeek d xs) return a

retryPoke :: Monad m => Delay -> [Glance m l o (Sealed m)] -> Pianola m l o () 
retryPoke d xs = retryPeek d xs >>= respond 

sleep :: Monad m => Delay -> Pianola m l o ()
sleep = lift . respond 

with :: (Functor m, Monad m) => Glance m l o' o ->  Pianola m l o a -> Pianola m l o' a 
with prefix pi  =
    hoist (hoist (mapMaybeT (hoist $ focus prefix))) $ pi 

infixl 0 `with`

instance Monad m => PianolaLog (Pianola m LogEntry o) where
    xanlog = lift . lift . lift . xanlog
