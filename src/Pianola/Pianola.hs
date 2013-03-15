{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}
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
        pmaybe,
        peek,
        peekMaybe,
        retryPeek,
        retryPeek1s,
        withMaybe,
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
import Data.Functor.Compose
import Data.Monoid
import Control.Category
import Control.Error
import Control.Applicative
import Control.Proxy
import Control.Monad
import Control.Monad.Free
import Control.Monad.Logic

import Pianola.Util

-- | A Glance is just a kleisli arrow used to locate and extract particular
-- elements of type /a/ in a data structure of type /o/. 
--
-- The following effects are allowed: 
--
--       * Nondeterminism and failure. A Glance can return more than one value (or zero
--       values, with 'mzero'). See the 'replusify' function, which is a valid Glance.
--
--       * Logging. A Glance can log messages of type /l/ about the elements it
--       encounters during search, even elements visited in search branches which
--       ultimately fail to produce any results. See 'logmsg'.
--
--       * Interactions  with the server through the monad /m/, but only interactions
--       that don't change the state of the GUI. For example, getting a image capture
--       of a window. See 'Nullipotent'.
--     
-- The following effects are forbidden: 
--
--       * Any kind of delay effect. Glances must return as soon as possible.  
--
--       * Interactions with the server which /do/ change the state of the GUI. Note
--       that you can target and return the actions of type 'Sealed' which dangle
--       on the branches of the source data structure. You just can't execute them
--       inside a Glance. To actually execute them, pass the glance as an argument to
--       'poke'.
type Glance m l o a = o -> LogicT (Produ l (Nullipotent m)) a

-- | Takes all the values returned by a 'Glance' and returns a new Glance in
-- which those values have been collected in a 'MonadPlus' (often a list). This
-- is useful when we want to recover a list of components which meet certain
-- criteria in order to compare them among themselves. For example, getting all
-- the buttons present in a window and sorting them left to right by their
-- position on the screen.
collect :: (Monad m, MonadPlus n) => Glance m l o a -> Glance m l o (n a)
collect = fmap $ \x -> lift $ observeAllT x >>= return . replusify

-- | Executes a 'Nullipotent' action in the context of a 'Glance'.
liftN :: Monad m => Glance m l (Nullipotent m a) a
liftN = lift . lift

-- | When the 'Glance' passed as argument finds nothing, the returned glance
-- finds a single (). When the Glance passed as argument finds one or more
-- values, the returned Glance finds zero results.
--
-- This function can be used in combination with 'retryPeek1s' to wait for a
-- component on screen to disappear.
missing :: Monad m => Glance m l o a -> Glance m l o () 
missing = fmap lnot

-- A Glance wrapped in a constructor to make it an instance of Functor.
type ObserverF m l o = Compose ((->) o) (LogicT (Produ l (Nullipotent m)))

-- A bunch of Glances chained together.  
type Observer m l o = Free (ObserverF m l o)

-- Transforms the context of an Observer by prepending a Glance from the new,
-- more general context to the old context to all the Glances contained in the
-- Observer.
focus :: Monad m => Glance m l o' o -> Observer m l o a -> Observer m l o' a
focus prefix v =
   let nattrans (Compose k) = Compose $ prefix >=> k
   in hoistFree nattrans v

-- Uses the value of type m o to unwind all the Glances in an Observer. When
-- one Glance returns with more than one result, one of the results is selected
-- in order to continue. Also, the Nullipotent restriction is removed. 
runObserver :: Monad m => m o -> Observer m l o a -> MaybeT (Produ l m) a
runObserver _ (Pure b) = return b
runObserver mom (Free f) =
   let squint = fmap $ hoist (hoist runNullipotent) . tomaybet
   in join $ (lift . lift $ mom) >>= squint (getCompose $ runObserver mom <$> f)

type Delay = Int

-- | The monad in which interactions with the GUI take place. The following
-- effects are allowed:
--
--       * Purely observational interations with the GUI. See 'peek'.
--      
--       * Logging. Log messages are emitted in the middle of the computation, unlike
--       in a Writer monad. See 'logmsg' and 'logimg'. 
--      
--       * Failure. See 'pfail'.
--      
--       * Delays. See 'sleep'.
--      
--       * Actions in the /m/ monad which actually change the GUI, like
--       clicking on a button. See 'poke'.
--
-- Instead of baking all possible effects into the base free monad, Pianola
-- takes the approach of representing each effect using the 'Proxy' type from
-- the pipes package.
--
-- The order of the trasformers in the monad stack is not arbitrary. For
-- example: it does not make sense for a log message to make the
-- computation fail or to trigger actions against the GUI, so the log producer
-- is colocated closest to the base monad, where it doesn't have access to
-- those kind of effects. 
--
-- Another example: it can be conveniento to automatically introduce
-- induce a delay after every action (see 'ralentize') or to automatically log
-- them (see 'autolog').  Therefore, the 'Sealed' action producer is in the
-- outermost position, having access to all the effects.
--
-- To actually execute a Pianola, use a driver function like
-- 'Pianola.Pianola.Driver.simpleDriver' or a specialization of it.
newtype Pianola m l o a = Pianola 
    { unPianola :: Produ (Sealed m) (Produ Delay (MaybeT (Produ l (Observer m l o)))) a 
    } deriving (Functor,Monad)

instance Monad m => Loggy (Pianola m LogEntry o) where
    logentry = Pianola . lift . lift . lift . logentry

-- | Unwinds all the Glances contained in a 'Pianola' by supplying them with
-- the monadic value passed as the first argument. When a 'Glance' returns with
-- more than one result, one of the results is selected in order to continue (/TO DO/: 
-- emit a warning when this happens). The log messages of the glances are
-- fused with the Pianola's own log stream. All the 'Sealed' actions are
-- injected into the base monad. The delay and log effects remain uninjected.
--
-- Usually, clients should not call this function directly, but use a
-- driver function like 'Pianola.Pianola.Driver.simpleDriver'.
play :: Monad m => m o -> Pianola m l o a -> Produ Delay (MaybeT (Produ l m)) a
play mom pi =
    let smashMaybe m () = runMaybeT m >>= lift . hoistMaybe
        smashProducer () = forever $
                request () >>= lift . lift . respond
        smash :: Monad m => MaybeT (Produ l (MaybeT (Produ l m))) a -> MaybeT (Produ l m) a
        smash mp = runProxy $ smashMaybe mp >-> smashProducer
        pi' = hoist (hoist (smash . hoist (hoist $ runObserver mom))) $ unPianola pi 
        injector () = forever $ do
            s <- request ()
            lift . lift . lift . lift $ unseal s
    in runProxy $ const pi' >-> injector

-- | Aborts a 'Pianola' computation.
pfail :: Monad m => Pianola m l o a
pfail = Pianola . lift . lift $ mzero

-- | If the second argument 'Pianola' returns Nothing, the first one is executed.
-- Often used in combination with 'pfail'. 
pmaybe :: Monad m => Pianola m l o a -> Pianola m l o (Maybe a) -> Pianola m l o a  
pmaybe f p = p >>= maybe f return 

-- | Lifts a 'Glance' into the 'Pianola' monad.
peek :: Monad m => Glance m l o a -> Pianola m l o a
peek = Pianola . lift . lift . lift . lift . liftF . Compose

-- | Like 'peek', but if the 'Glance' returns zero results then Nothing is returned instead of failing and halting the whole computation. 
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

withMaybe :: Monad m => Glance m l o' o -> Pianola m l o a -> Pianola m l o' (Maybe a) 
withMaybe glance pi = do
    r <- peekMaybe glance 
    case r of 
        Nothing -> return Nothing
        Just _ -> with glance pi >>= return . Just

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

-- | Takes a glance that extracts an action of type 'Sealed' from a data
-- structure, and returns a 'Pianola' executing the action (when the Pianola is
-- interpreted by some driver-like fuction like
-- 'Pianola.Pianola.Driver.simpleDriver'.)
poke :: Monad m => Glance m l o (Sealed m) -> Pianola m l o () 
poke locator = peek locator >>= inject

-- | Like 'poke', but if the 'Glance' returns zero results then Nothing is
-- returned instead of failing and halting the whole computation. 
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

-- | Sleeps for the specified number of seconds
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
    
-- | Takes a delay in seconds and a 'Pianola' as parameters, and returns a
-- ralentized Pianola in which the delay has been inserted after every action.
ralentize :: Delay -> Pianola m l o a -> Pianola m l o a
ralentize = ralentizeByTag $ const True
    
-- | Modifies a 'Pianola' so that the default tags associated to an action are
-- logged automatically when the action is executed.
autolog :: Pianola m LogEntry o a -> Pianola m LogEntry o a 
autolog (Pianola p) =
    let logger () = forever $ do
            s <- request ()
            respond s
            lift . lift . lift . logmsg $ fmtAction s
        fmtAction s = 
            "### Executed action with tags:" <> mconcat ( map (" "<>) . tags $ s ) 
    in Pianola $ const p >-> logger $ ()

