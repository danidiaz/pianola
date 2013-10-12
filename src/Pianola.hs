{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pianola (
        Selector(..),
        missing,
        winnow,
        context,
        collect,
        liftQ,
        Pianola(..),
        Delay,
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
        feed
    ) where

import Prelude hiding (catch,(.))
import Data.Functor.Compose
import Data.Monoid
import Control.Category
import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Free
import Control.Monad.Logic
import Control.Monad.Error
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Comonad
import Control.Comonad.Trans.Env    
import Pipes

import Pianola.Util

--type Selector m l o a = o -> LogicT (Producer l (Query m)) a
type Selector m l o a = Kleisli (LogicT (Producer l (Query m))) o a

collect :: (Monad m, MonadPlus n) => Selector m l o a -> Selector m l o (n a)
collect = Kleisli . fmap f . runKleisli
    where f x = lift $ observeAllT x >>= return . replusify

liftQ :: Monad m => Selector m l (Query m a) a
liftQ = Kleisli $ lift . lift

missing :: Monad m => Selector m l o a -> Selector m l o () 
missing k = Kleisli $ fmap lnot $ runKleisli k

winnow :: Monad m => Selector m l o a -> Selector m l o a
winnow k = Kleisli $ fmap once $ runKleisli k

-- arr ask would do the same job, if working with arrows...
context :: (Comonad c, Monad m) => Kleisli m (EnvT e c a) e
context = arr ask

--type ObserverF m l o = Compose ((->) o) (LogicT (Producer l (Query m)))
type ObserverF m l o = WrappedArrow (Kleisli (LogicT (Producer l (Query m)))) o

type Observer m l o = Free (ObserverF m l o)

focus :: Monad m => Selector m l o' o -> Observer m l o a -> Observer m l o' a
focus prefix v =
   let nattrans (WrapArrow k) = WrapArrow $ prefix >>> k
   in  hoistFree nattrans v

runObserver :: Monad m => m o -> Observer m l o a -> ErrorT String (Producer l m) a
runObserver _ (Pure b) = return b
runObserver mom (Free f) =
   let  tomaybet = MaybeT . liftM replusify . observeManyT 1
        toerrort = ErrorT . liftM (maybe (Left "# Selector without results.") Right) . runMaybeT
        squint = fmap $ hoist (hoist runQuery) . toerrort . tomaybet
   in join $ (lift . lift $ mom) >>= (squint . runKleisli . unwrapArrow $ runObserver mom <$> f)

type Delay = Int

newtype Pianola m l o a = Pianola 
    { unPianola :: Producer (Change m) (Producer Delay (ErrorT String (Producer l (Observer m l o)))) a 
    } deriving (Functor,Monad,MonadError String)

instance Monad m => Loggy (Pianola m LogEntry o) where
    logentry = Pianola . lift . lift . lift . logentry

peek :: Monad m => Selector m l o a -> Pianola m l o a
peek = Pianola . lift . lift . lift . lift . liftF . WrapArrow

peekMaybe :: Monad m => Selector m l o a -> Pianola m l o (Maybe a)
peekMaybe = peek . collect

retryPeek1s :: Monad m => Int -> Selector m l o a -> Pianola m l o (Maybe a)
retryPeek1s = retryPeek $ sleep 1

retryPeek :: Monad m => Pianola m l o u -> Int -> Selector m l o a -> Pianola m l o (Maybe a)
retryPeek delay times glance =
    let retryPeek' [] = return Nothing
        retryPeek' (x:xs) = do
            z <- peekMaybe x
            maybe (delay >> retryPeek' xs) (return.return) z 
    in retryPeek' $ replicate times glance


inject :: Monad m => Change m -> Pianola m l o ()
inject = Pianola . yield

poke :: Monad m => Selector m l o (Change m) -> Pianola m l o () 
poke locator = peek locator >>= inject

pokeMaybe :: Monad m => Selector m l o (Change m) -> Pianola m l o (Maybe ())
pokeMaybe locator = do 
    actionMaybe <- peekMaybe locator 
    case actionMaybe of
        Nothing -> return Nothing
        Just action -> inject action >> return (Just ())

retryPoke1s :: Monad m => Int -> Selector m l o (Change m)  -> Pianola m l o (Maybe ())
retryPoke1s = retryPoke $ sleep 1

retryPoke :: Monad m => Pianola m l o u -> Int -> Selector m l o (Change m)  -> Pianola m l o (Maybe ())
retryPoke delay times glance = do
    actionMaybe <- retryPeek delay times glance
    case actionMaybe of
       Nothing -> return Nothing
       Just action -> inject action >> return (Just ())

sleep :: Monad m => Delay -> Pianola m l o ()
sleep = Pianola . lift . yield

with :: Monad m => Selector m l o' o -> Pianola m l o a -> Pianola m l o' a 
with prefix pi  =
    Pianola $ hoist (hoist (hoist (hoist $ focus prefix))) $ unPianola pi 

withMaybe :: Monad m => Selector m l o' o -> Pianola m l o a -> Pianola m l o' (Maybe a) 
withMaybe glance pi = do
    r <- peekMaybe glance 
    case r of 
        Nothing -> return Nothing
        Just _ -> with glance pi >>= return . Just

withRetry1s :: Monad m => Int -> Selector m l o' o -> Pianola m l o a -> Pianola m l o' (Maybe a)
withRetry1s = withRetry $ sleep 1

withRetry :: Monad m => Pianola m l o' u -> Int -> Selector m l o' o -> Pianola m l o a -> Pianola m l o' (Maybe a)
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

feed :: Monad m => m o -> Pianola m l o a -> Producer Delay (ErrorT String (Producer l m)) a
feed mom pi =
    let smashMaybe m = runErrorT m >>= lift . ErrorT . return
        smashProducer = forever $
                await >>= lift . lift . yield
        smash mp = runEffect $ smashMaybe mp >-> smashProducer
        pi' = hoist (hoist (smash . hoist (hoist $ runObserver mom))) $ unPianola pi 
        injector = forever $ do
            s <- await
            lift . lift . lift . lift $ unseal s
    in runEffect $ pi' >-> injector

