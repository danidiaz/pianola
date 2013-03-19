module Pianola.Tutorial (
    -- * Setting up the test application
    -- $setup
    
    -- * First steps
    -- $firststeps

    -- * Logging
    -- $logging

    -- * Clicking buttons
    -- $clicking

    -- * Grouping actions
    -- $grouping
    
    -- * Logging, again
    -- $autologging
    
    -- * Sleeping
    -- $sleeping

    -- * Going slow
    -- $ralentizing
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

{- $clicking
Enough with just inspecting the GUI and logging the results! What if we actually want to effect some change, like clicking a button? For this, we can use the 'poke' function.

> clicky :: Pianola Protocol LogEntry (GUI Protocol) ()
> clicky = poke $ mainWindow >=> contentPane >=> descendants >=> hasText (=="open dialog") >=> clickButton  

Like 'peek', 'poke' takes a 'Glance' as a parameter. Unlike 'peek', it only accepts Glances which return an action of type 'Sealed'. Values of this type encapsulate actions to be performed on the GUI. Users never construct values of 'Sealed'. Instead, they find them while exploring the data structure which represents the GUI.

In the example, the 'Glance' supplied to 'poke' is made up of smaller Glances combined with '>=>'. One which extracts the main window of the application from the GUI as a whole, one to access the content pane of the window, one for obtaining all the sub-components of the content pane, one that selects components according to their text content, and finally one which extracts the click action of the component if the component is a button.
-}


{- $grouping
Suppose we want to click the clear button in the window before we click the open dialog button. We can do it like this:

> groupy :: Pianola Protocol LogEntry (GUI Protocol) ()
> groupy = do
>    poke $ mainWindow >=> contentPane >=> descendants >=> hasText (=="clear") >=> clickButton  
>    poke $ mainWindow >=> contentPane >=> descendants >=> hasText (=="open dialog") >=> clickButton  

There is a lot of repetition in the Glances. We can factor it out with the 'with' function:

> groupy2 :: Pianola Protocol LogEntry (GUI Protocol) ()
> groupy2 = with (mainWindow >=> contentPane >=> descendants) $ do
>    poke $ hasText (=="clear") >=> clickButton  
>    poke $ hasText (=="open dialog") >=> clickButton  

This is equivalent to the previous code:

> groupy3 :: Pianola Protocol LogEntry (GUI Protocol) ()
> groupy3 = with mainWindow $ with contentPane $ with descendants $ do
>    poke $ hasText (=="clear") >=> clickButton  
>    poke $ hasText (=="open dialog") >=> clickButton  

In this case, we could even go a little further:

> groupy4 :: Pianola Protocol LogEntry (GUI Protocol) ()
> groupy4 =
>    with mainWindow $ with contentPane $ with descendants $ do
>       forM_ ["clear","open dialog] $ \txt ->
>          poke $ hasText (==txt) >=> clickButton 

-}

{- $autologging
-}

{- $sleeping
-}

{- $ralentizing
-}
