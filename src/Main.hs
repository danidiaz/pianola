{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Prelude hiding (catch,(.))
import System.IO
import System.Environment
import System.Console.GetOpt
import Data.Tree
import qualified Data.Text as T
import Network 
import Control.Category
import Control.Error
import Control.Applicative
import Control.Proxy
import Control.Monad
import Control.Exception
import Control.Monad.Base
import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.Logic

import Xanela.Util
import Xanela.Types
import Xanela.Types.Protocol
import Xanela.Types.Protocol.IO
 
type Search m = LogicT (EitherT AssertError (Producer LogEntry m))

data AssertError = AssertError T.Text

type LogEntry = T.Text 

instance MonadBase b m => MonadBase b (Search m) where
    liftBase = lift.lift.lift.liftBase

type TestCase = MonadBase n (Search m) => GUI n -> Search m ()

testCase:: TestCase
testCase = prefixK_ ( wait t >=> windowsflat ) [
                    contentsflat >=> text "foo" >=> click,
                    contentsflat >=> text "dialog button" >=> click,
                    menuflat >=> text "Menu1" >=> click,
                    popupflat >=> text "SubMenu1" >=> click,
                    popupflat >=> text "submenuitem2" >=> toggle False
                ]
           >=> unitK
    where t = 2

main :: IO ()
main = do
  args <- getArgs 
  let addr = head args
      port = PortNumber . fromIntegral $ 26060
      endpoint = Endpoint addr port

      test:: Search Protocol ()
      test = liftBase getgui >>= testCase

      producer:: Producer LogEntry Protocol (Either AssertError [()])
      producer = runEitherT . observeAllT $ test

      producerio = mapFreeT runProtocol $ producer 
      eerio = runPipe $ producerio >+> discard ()
  r <- flip runReaderT endpoint . runEitherT . runEitherT $ eerio
  case r of
        Left _ -> putStrLn "io error"
        Right r2 -> case r2 of
            Left _ -> putStrLn "procotol error"
            Right r3 -> case r3 of
                Left _ -> putStrLn "assertion error"
                Right u -> case u of
                    [()] -> putStrLn "all ok, only one result"
                    _ -> putStrLn "oops this shouldn't happen"
