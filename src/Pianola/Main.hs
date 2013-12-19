{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Pianola.Main (
        makeMain
    ) where

import Prelude hiding (catch,(.),id)
import Control.Category
import Data.Functor.Identity
import Data.Tree
import Data.Foldable (toList)
import Data.Monoid
import Data.MessagePack
import Data.Attoparsec.ByteString
import Control.Lens
import Control.Arrow
import Control.Monad
import Control.Comonad
import Control.Comonad.Trans.Class
import Control.Comonad.Trans.Env    
import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Logic
import Control.Monad.Error
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Options.Applicative as O
import Pipes

import Pianola.Internal

data Command = ListC  
             | RunC Run
    deriving Show
 
data Run = Run 
        { testName :: String
        , hostName :: String
        , port :: Int
        } 
    deriving Show

-- If using O.subparser instead of O.hsubparser, 
-- you'll need to add O.helper to each sub-command parser.
parser :: O.ParserInfo Command
parser = O.info (O.helper <*> O.hsubparser subParsers) parserModifiers
    where 
    subParsers = listCommand <> runCommand 
    listCommand = O.command "list" (O.info (pure ListC) listModifiers)
    listModifiers = mconcat
        [ O.fullDesc 
        , O.header "This is the header" 
        , O.footer "This is the footer"
        , O.progDesc "This is the program description"
        ] 
    runCommand = O.command "run" (O.info (RunC <$> runParser) runModifiers)
    runParser = Run <$> testName <*> machineOption <*> portOption
    testName = O.argument O.str $ mconcat 
        [ O.metavar "TEST"
        , O.help "Name of the text to execute"
        ]
    machineOption = O.strOption $ mconcat
        [ O.value "localhost" 
        , O.showDefault 
        , O.short 'm' 
        , O.metavar "MACHINE" 
        , O.help "GUI agent location"
        ] 
    portOption = O.option $ mconcat
        [ O.value 26060
        , O.showDefault
        , O.short 'p' 
        , O.metavar "PORT"
        , O.help "GUI agent listening port"
        ]   
    runModifiers = mconcat
        [ O.header "blah blach"
        , O.footer "footer for run"
        ]
    parserModifiers = mconcat
        [ O.fullDesc 
        , O.progDesc "main prog desc" 
        , O.header "hello - a test for optparse-applicative"
        ]


--testCase:: Monad m => Remote m -> Pianola m LogEntry GUI () 
--testCase p = with (decorate $ topLevel.folded) $ do

makeMain :: IO ()
makeMain = undefined



