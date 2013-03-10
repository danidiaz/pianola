module Pianola.Tutorial (
    -- * Setting up the test application
    -- $setup
    
    -- * First steps
    -- $firststeps

    -- * Logging
    -- $logging

) where 

import Prelude hiding (catch,(.),id,head,repeat,tail,map,iterate)
import Data.Stream.Infinite
import Control.Error
import Pianola.Util
import Pianola.Protocol
import Pianola.Protocol.IO
import Pianola.Pianola
import Pianola.Pianola.Driver
import Pianola.Model.Swing
import Pianola.Model.Swing.Driver

{- $setup
    This tutorial assumes that the Java Swing test application bundled with the
package is up and running. To set up the application, unpack the package
sources with 

>>> cabal unpack pianola 

Then go to the 

>>> pianola/backends/java-swing 

folder and follow the instructions in the @README@ to compile and install the pianola agent jar into a local
Maven repository. Finally, go to the 

>>> pianola/backends/java-swing-testapp 

folder and follow the instructions in the @README@ to configure and launch the test application.

This tutorial will refer to the Java Swing application as the Application Under Test (AUT).
-}
{- $firststeps
    This minimal pianola script prints the title of the AUT's main window: 

> import qualified Data.Text as T
> import Network 
> import Control.Monad
> import Control.Error
> 
> import Pianola.Model.Swing.Driver
> 
> extractTitle :: Pianola Protocol LogEntry (GUI Protocol) T.Text
> extractTitle = peek $ mainWindow >=> title  
> 
> main:: IO ()
> main = do 
>     let port =  PortNumber . fromIntegral $ 26060
>         endpoint = Endpoint "127.0.0.1" port
>         screenshots = screenshotStream "/tmp"   
>     r <- runEitherT $ simpleSwingDriver endpoint extractTitle screenshots
>     putStrLn $ case r of
>        Left err -> do
>           "ERROR: " ++ show err
>        Right title -> "title: " ++ show title
-}

{- $logging
We could also have logged the title inside the 'Pianola' monad:

> extractTitle2 :: Pianola Protocol LogEntry (GUI Protocol) ()
> extractTitle2 = do 
>     txt <- peek $ mainWindow >=> title  
>     logmsg txt

Log messages emitted in the Pianola monad are printed as they are generated, unlike in a Writer monad. This is useful to check the progress of long-running scripts.

The expression to the right of 'peek' has type 'Glance'. Glance is just a type synonym for the Kleisli arrows of a particular monad. This monad allows some effects and disallows others. Turns out that logging is one of the allowed effects, so we can also emit messages inside a Glance: 

> import Data.Functor.Identity
>
> extractTitle3 :: Pianola Protocol LogEntry (GUI Protocol) ()
> extractTitle3 = do 
>     peek $ \gui -> do
>         win <- mainWindow gui 
>         logmsg $ runIdentity $ title win

Why bother at all with logging inside a 'Glance', instead of always doing it in the 'Pianola' monad? As it happens, nondeterminism (returning several, or zero, results) is another of the allowed effects inside Glances. We can log about objects explored in search branches even if those branches eventually fail to produce any result. This can be useful for debugging.
-}







