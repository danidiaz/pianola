import Data.List
import Options.Applicative as O
import System.Process
import System.FilePath
import System.Directory

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
        agentfolder = joinPath ["backends", "java-swing"]
    withDirectory agentfolder maven
    let upThree = replicate 3 ".." 
        classpath = intercalate ";" . map joinPath $  
            [ upThree ++ ["backends","java-swing","target","dependency","*"] 
            , ["target","*"] 
            ]
        agentpath = joinPath $ upThree ++
            ["backends","java-swing","target","pianola-driver-1.0.jar"]
        agentargs = (++) "=port/26060,popupTrigger/" $ case os of
            Windows -> "release"
            Linux   -> "press"
        agentclass = "info.danidiaz.pianola.testapp.Main"
        appfolder = joinPath ["integration","apps","java-swing-testapp"]
    handle <- withDirectory appfolder $ do
        maven            
        spawnProcess "java" ["-cp", classpath
                            ,"-javaagent:" ++ agentpath ++ agentargs
                            , agentclass
                            ]  
    readProcess "cabal" ["install","--enable-tests"] [] >>= putStrLn
    terminateProcess handle
