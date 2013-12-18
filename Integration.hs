import Data.List
import Data.Foldable
import System.Exit
import System.Exit.Lens
import System.Process
import System.FilePath
import System.Directory

import Control.Lens
import Control.Concurrent
import Options.Applicative as O

withDirectory :: FilePath -> IO a -> IO a
withDirectory path action = do
    current <- getCurrentDirectory 
    setCurrentDirectory path *> action <* setCurrentDirectory current

invoke :: FilePath -> [String] -> IO ()
invoke file args = do 
    (exitCode,stdout,stderr) <- readProcessWithExitCode file args ""
    putStrLn stdout
    putStrLn stderr
    forOf_ _ExitFailure exitCode $ \_ -> putStrLn "Oops" >> exitFailure 

cabal :: Bool -> IO ()
cabal shouldEnable = invoke "cabal" $ ["install"] ++ enable
    where
    enable = if shouldEnable then  ["--enable-tests"] else []

maven :: IO ()
maven = invoke "mvn.bat" ["install"] 

data OS = Linux
        | Windows

main :: IO ()
main = do
    let parser = O.switch (O.long "windows")
    b <- O.execParser $ O.info (O.helper <*> parser) O.fullDesc
    let os = if b then Windows else Linux 
        agentfolder = joinPath ["backends", "java-swing"]
        threeUp = replicate 3 ".." 
        classpath = intercalate ";" . map joinPath $  
            [ threeUp ++ ["backends","java-swing","target","dependency","*"] 
            , ["target","*"] 
            ]
        agentpath = joinPath $ threeUp ++
            ["backends","java-swing","target","pianola-driver-1.0.jar"]
        agentargs = (++) "=port/26060,popupTrigger/" $ case os of
            Windows -> "release"
            Linux   -> "press"
        agentclass = "info.danidiaz.pianola.testapp.Main"
        appfolder = joinPath ["integration","apps","java-swing-testapp"]
    withDirectory agentfolder   $ maven 
    withDirectory appfolder     $ maven
    withDirectory "."           $ cabal True 
    withDirectory "integration" $ cabal False
    handle <- withDirectory appfolder $ 
        spawnProcess "java" ["-cp", classpath
                            ,"-javaagent:" ++ agentpath ++ agentargs
                            , agentclass
                            ]  
    threadDelay $ 10^6*2
    invoke "pianola-integration-test-app" []
    terminateProcess handle
    putStrLn "PASS" 
