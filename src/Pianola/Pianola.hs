{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pianola.Pianola (
        Glance(..),
        missing,
        collect,
        liftN,
        Pianola(..),
        Delay,
        play,
        pfail,
        pfailMaybe,
        peek,
        peekMaybe,
        retryPeek,
        retryPeek1s,
        withRetry,
        withRetry1s,
        poke,
        pokeMaybe,
        retryPoke,
        retryPoke1s,
        sleep,
        with,
        ralentizeByTag,
        ralentize,
        autolog
    ) where

import Prelude hiding (catch,(.))
import System.IO
import System.Environment
import System.Console.GetOpt
import Data.Tree
import Data.Functor.Compose
import Data.Monoid
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
import Control.Monad.Free
import Control.Monad.Trans.Reader
import Control.Monad.Logic
import Control.Monad.Trans.Maybe

import Pianola.Util

type Glance m l o a = o -> LogicT (Prod l (Nullipotent m)) a

collect :: (Monad m, MonadPlus n) => Glance m l o a -> Glance m l o (n a)
collect = fmap $ \x -> lift $ observeAllT x >>= return . replusify

liftN :: Monad m => Glance m l (Nullipotent m a) a
liftN = lift . lift

missing :: Monad m => Glance m l o a -> Glance m l o () 
missing = fmap lnot

type ObserverF m l o = Compose ((->) o) (LogicT (Prod l (Nullipotent m)))

type Observer m l o = Free (ObserverF m l o)

focus :: Monad m => Glance m l o' o -> Observer m l o a -> Observer m l o' a
focus prefix v =
   let nattrans (Compose k) = Compose $ prefix >=> k
   in hoistFree nattrans v

runObserver :: Monad m => m o -> Observer m l o a -> MaybeT (Prod l m) a
runObserver _ (Pure b) = return b
runObserver mom (Free f) =
   let squint = fmap $ hoist (hoist runNullipotent) . tomaybet
   in join $ (lift . lift $ mom) >>= squint (getCompose $ runObserver mom <$> f)

type Delay = Int

newtype Pianola m l o a = Pianola { unPianola :: Prod (Sealed m) (Prod Delay (MaybeT (Prod l (Observer m l o)))) a } deriving (Functor,Monad)

instance Monad m => Loggy (Pianola m LogEntry o) where
    xanlog = Pianola . lift . lift . lift . xanlog

play :: Monad m => m o -> Pianola m l o a -> Prod Delay (MaybeT (Prod l m)) a
play mom pi =
    let smashMaybe m () = runMaybeT m >>= lift . hoistMaybe
        smashProducer () = forever $
                request () >>= lift . lift . respond
        smash :: Monad m => MaybeT (Prod l (MaybeT (Prod l m))) a -> MaybeT (Prod l m) a
        smash mp = runProxy $ smashMaybe mp >-> smashProducer
        pi' = hoist (hoist (smash . hoist (hoist $ runObserver mom))) $ unPianola pi 
        injector () = forever $ do
            s <- request ()
            lift . lift . lift . lift $ unseal s
    in runProxy $ const pi' >-> injector

pfail :: Monad m => Pianola m l o a
pfail = Pianola . lift . lift $ mzero

pfailMaybe :: Monad m => Pianola m l o (Maybe a) -> Pianola m l o a  
pfailMaybe p = p >>= maybe pfail return 

peek :: Monad m => Glance m l o a -> Pianola m l o a
peek = Pianola . lift . lift . lift . lift . liftF . Compose

peekMaybe :: Monad m => Glance m l o a -> Pianola m l o (Maybe a)
peekMaybe = peek . collect

retryPeek :: Monad m => Pianola m l o u -> Int -> Glance m l o a -> Pianola m l o (Maybe a)
retryPeek delay times glance =
    let retryPeek' [] = return Nothing
        retryPeek' (x:xs) = do
            z <- peekMaybe x
            maybe (delay >> retryPeek' xs) (return.return) z 
    in retryPeek' $ replicate times glance

retryPeek1s :: Monad m => Int -> Glance m l o a -> Pianola m l o (Maybe a)
retryPeek1s = retryPeek $ sleep 1

withRetry :: Monad m => Pianola m l o' u -> Int -> Glance m l o' o -> Pianola m l o a -> Pianola m l o' (Maybe a)
withRetry delay times glance pi = do
    r <- retryPeek delay times glance 
    case r of 
        Nothing -> return Nothing
        Just _ -> with glance pi >>= return . Just

withRetry1s :: Monad m => Int -> Glance m l o' o -> Pianola m l o a -> Pianola m l o' (Maybe a)
withRetry1s = withRetry $ sleep 1

inject :: Monad m => Sealed m -> Pianola m l o ()
inject = Pianola . respond 

poke :: Monad m => Glance m l o (Sealed m) -> Pianola m l o () 
poke locator = peek locator >>= inject

pokeMaybe :: Monad m => Glance m l o (Sealed m) -> Pianola m l o (Maybe ())
pokeMaybe locator = do 
    actionMaybe <- peekMaybe locator 
    case actionMaybe of
        Nothing -> return Nothing
        Just action -> inject action >> return (Just ())

retryPoke :: Monad m => Pianola m l o u -> Int -> Glance m l o (Sealed m)  -> Pianola m l o (Maybe ())
retryPoke delay times glance = do
    actionMaybe <- retryPeek delay times glance
    case actionMaybe of
       Nothing -> return Nothing
       Just action -> inject action >> return (Just ())

retryPoke1s :: Monad m => Int -> Glance m l o (Sealed m)  -> Pianola m l o (Maybe ())
retryPoke1s = retryPoke $ sleep 1

sleep :: Monad m => Delay -> Pianola m l o ()
sleep = Pianola . lift . respond 

with :: Monad m => Glance m l o' o -> Pianola m l o a -> Pianola m l o' a 
with prefix pi  =
    Pianola $ hoist (hoist (hoist (hoist $ focus prefix))) $ unPianola pi 

ralentizeByTag :: ([Tag] -> Bool) -> Delay -> Pianola m l o a -> Pianola m l o a
ralentizeByTag f delay (Pianola p) = 
    let delayer () = forever $ do  
            s <- request ()
            respond s
            when (f . tags $ s) (lift $ respond delay) 
    in Pianola $ const p >-> delayer $ ()
    
ralentize :: Delay -> Pianola m l o a -> Pianola m l o a
ralentize = ralentizeByTag $ const True
    
autolog :: Pianola m LogEntry o a -> Pianola m LogEntry o a 
autolog (Pianola p) =
    let logger () = forever $ do
            s <- request ()
            respond s
            lift . lift . lift . logmsg $ fmtAction s
        fmtAction s = 
            "### Executed action with tags:" <> mconcat ( map (" "<>) . tags $ s ) 
    in Pianola $ const p >-> logger $ ()

