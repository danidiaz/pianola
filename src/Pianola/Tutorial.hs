module Pianola.Tutorial (
    -- * Setting up the test application
    -- $setup
    
    -- * First steps
    -- $firststeps
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







