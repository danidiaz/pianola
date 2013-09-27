{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pianola (
        Glance(..),
        missing,
        collect,
        liftN,
        Pianola(..),
        Delay,
        pfail,
        pmaybe,
        peek,
        peekMaybe,
        retryPeek1s,
        retryPeek,
        poke,
        pokeMaybe,
        retryPoke1s,
        retryPoke,
        sleep,
        with,
        withMaybe,
        withRetry1s,
        withRetry,
        ralentize,
        ralentizeByTag,
        autolog,
        play
    ) where

import Prelude hiding (catch,(.))
import Data.Functor.Compose
import Data.Monoid
import Control.Category
import Control.Error
import Control.Applicative
import Control.Monad
import Control.Monad.Free
import Control.Monad.Logic
import Pipes

import Pianola.Util

type Glance m l o a = o -> LogicT (Producer l (Nullipotent m)) a

collect :: (Monad m, MonadPlus n) => Glance m l o a -> Glance m l o (n a)
collect = fmap $ \x -> lift $ observeAllT x >>= return . replusify

liftN :: Monad m => Glance m l (Nullipotent m a) a
liftN = lift . lift

missing :: Monad m => Glance m l o a -> Glance m l o () 
missing = fmap lnot

type ObserverF m l o = Compose ((->) o) (LogicT (Producer l (Nullipotent m)))

type Observer m l o = Free (ObserverF m l o)

focus :: Monad m => Glance m l o' o -> Observer m l o a -> Observer m l o' a
focus prefix v =
   let nattrans (Compose k) = Compose $ prefix >=> k
   in hoistFree nattrans v

runObserver :: Monad m => m o -> Observer m l o a -> MaybeT (Producer l m) a
runObserver _ (Pure b) = return b
runObserver mom (Free f) =
   let squint = fmap $ hoist (hoist runNullipotent) . tomaybet
   in join $ (lift . lift $ mom) >>= squint (getCompose $ runObserver mom <$> f)

type Delay = Int

newtype Pianola m l o a = Pianola 
    { unPianola :: Producer (Sealed m) (Producer Delay (MaybeT (Producer l (Observer m l o)))) a 
    } deriving (Functor,Monad)

instance Monad m => Loggy (Pianola m LogEntry o) where
    logentry = Pianola . lift . lift . lift . logentry

pfail :: Monad m => Pianola m l o a
pfail = Pianola . lift . lift $ mzero

pmaybe :: Monad m => Pianola m l o a -> Pianola m l o (Maybe a) -> Pianola m l o a  
pmaybe f p = p >>= maybe f return 

peek :: Monad m => Glance m l o a -> Pianola m l o a
peek = Pianola . lift . lift . lift . lift . liftF . Compose

peekMaybe :: Monad m => Glance m l o a -> Pianola m l o (Maybe a)
peekMaybe = peek . collect

retryPeek1s :: Monad m => Int -> Glance m l o a -> Pianola m l o (Maybe a)
retryPeek1s = retryPeek $ sleep 1

retryPeek :: Monad m => Pianola m l o u -> Int -> Glance m l o a -> Pianola m l o (Maybe a)
retryPeek delay times glance =
    let retryPeek' [] = return Nothing
        retryPeek' (x:xs) = do
            z <- peekMaybe x
            maybe (delay >> retryPeek' xs) (return.return) z 
    in retryPeek' $ replicate times glance


inject :: Monad m => Sealed m -> Pianola m l o ()
inject = Pianola . yield

poke :: Monad m => Glance m l o (Sealed m) -> Pianola m l o () 
poke locator = peek locator >>= inject

pokeMaybe :: Monad m => Glance m l o (Sealed m) -> Pianola m l o (Maybe ())
pokeMaybe locator = do 
    actionMaybe <- peekMaybe locator 
    case actionMaybe of
        Nothing -> return Nothing
        Just action -> inject action >> return (Just ())

retryPoke1s :: Monad m => Int -> Glance m l o (Sealed m)  -> Pianola m l o (Maybe ())
retryPoke1s = retryPoke $ sleep 1

retryPoke :: Monad m => Pianola m l o u -> Int -> Glance m l o (Sealed m)  -> Pianola m l o (Maybe ())
retryPoke delay times glance = do
    actionMaybe <- retryPeek delay times glance
    case actionMaybe of
       Nothing -> return Nothing
       Just action -> inject action >> return (Just ())

sleep :: Monad m => Delay -> Pianola m l o ()
sleep = Pianola . lift . yield

with :: Monad m => Glance m l o' o -> Pianola m l o a -> Pianola m l o' a 
with prefix pi  =
    Pianola $ hoist (hoist (hoist (hoist $ focus prefix))) $ unPianola pi 

withMaybe :: Monad m => Glance m l o' o -> Pianola m l o a -> Pianola m l o' (Maybe a) 
withMaybe glance pi = do
    r <- peekMaybe glance 
    case r of 
        Nothing -> return Nothing
        Just _ -> with glance pi >>= return . Just

withRetry1s :: Monad m => Int -> Glance m l o' o -> Pianola m l o a -> Pianola m l o' (Maybe a)
withRetry1s = withRetry $ sleep 1

withRetry :: Monad m => Pianola m l o' u -> Int -> Glance m l o' o -> Pianola m l o a -> Pianola m l o' (Maybe a)
withRetry delay times glance pi = do
    r <- retryPeek delay times glance 
    case r of 
        Nothing -> return Nothing
        Just _ -> with glance pi >>= return . Just

ralentize :: Delay -> Pianola m l o a -> Pianola m l o a
ralentize = ralentizeByTag $ const True
    
ralentizeByTag :: ([Tag] -> Bool) -> Delay -> Pianola m l o a -> Pianola m l o a
ralentizeByTag f delay (Pianola p) = 
    let delayer = forever $ do  
            s <- await
            yield s
            when (f . tags $ s) (lift $ yield delay) 
    in Pianola $ p >-> delayer
    
autolog :: Pianola m LogEntry o a -> Pianola m LogEntry o a 
autolog (Pianola p) =
    let logger = forever $ do
            s <- await
            yield s
            lift . lift . lift . logmsg $ fmtAction s
        fmtAction s = 
            "### Executed action with tags:" <> mconcat ( map (" "<>) . tags $ s ) 
    in Pianola $ p >-> logger

play :: Monad m => m o -> Pianola m l o a -> Producer Delay (MaybeT (Producer l m)) a
play mom pi =
    let smashMaybe m = runMaybeT m >>= lift . hoistMaybe
        smashProducer = forever $
                await >>= lift . lift . yield
        smash mp = runEffect $ smashMaybe mp >-> smashProducer
        pi' = hoist (hoist (smash . hoist (hoist $ runObserver mom))) $ unPianola pi 
        injector = forever $ do
            s <- await
            lift . lift . lift . lift $ unseal s
    in runEffect $ pi' >-> injector

