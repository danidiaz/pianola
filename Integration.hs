module Integration ( main ) where

import Control.Applicative
import Options.Applicative as O
import System.Process
import System.Directory
import System.FilePath

withDirectory :: FilePath -> IO a -> IO a
withDirectory path action = do
    current <- getCurrentDirectory 
    setCurrentDirectory path *> action <* setCurrentDirectory current

maven :: IO ()
maven = readProcess "mvn.bat" ["install"] [] >>= putStrLn

data OS = Linux
        | Windows

main :: IO ()
main = do
    let parser = O.switch (O.long "windows")
    b <- O.execParser $ O.info (O.helper <*> parser) O.fullDesc
    let os = if b then Windows else Linux 
        backends = "backends"
        agentfolder = joinPath [backends, "java-swing"]
    withDirectory agentfolder $ maven
    let classpath = joinPath ["..","java-swing","target","dependency","*"] ++ ";" ++ 
                    joinPath ["target","*"]
        agentpath = joinPath ["..","java-swing","target","pianola-driver-1.0.jar"]
        agentargs = (++) "=port/26060,popupTrigger/" $ case os of
                        Windows -> "release"
                        Linux   -> "press"
        agentclass = "info.danidiaz.pianola.testapp.Main"
        appfolder = joinPath [backends, "java-swing-testapp"]
    handle <- withDirectory appfolder $ do
                maven            
                spawnProcess "java.exe" ["-cp", classpath, "-javaagent:" ++ agentpath ++ agentargs, agentclass]  
    readProcess "cabal.exe" ["install","--enable-tests"] [] >>= putStrLn
    terminateProcess handle
