{-# OPTIONS_GHC -XDeriveDataTypeable #-}
module Main (main) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM.TMVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM

import Data.Time.Format
import Data.Time.LocalTime

import System.Console.CmdArgs
import System.Environment
import System.Exit
import System.IO
import System.Locale
import System.Log.Logger
import System.Log.Handler.Simple

import StatusBar.Client
import StatusBar.Dzen
import StatusBar.Timer

-- Options to this program, gotten from the command line, a config file, or
-- something like that
data Options = Options
    { host  :: String -- The server host
    , port  :: String -- The server port
    , name_ :: String -- The client name
    }
    deriving (Show, Data, Typeable)

-- Set default options and annotations
options :: Options
options = Options
    { host = def &= argPos 0 &= opt "localhost"
    , port = "9999"
        &= help "The server port"
    , name_ = "Test"
        &= help "Specify a client name"
    }
    &= program "TestClient"
    &= summary "TestClient v0.1.0"
    &= help "A test client to connect to the StatusBar server"

main :: IO ()
main = do
    -- Set up logging
    logHandler <- verboseStreamHandler stderr DEBUG
    updateGlobalLogger rootLoggerName $ setLevel DEBUG . setHandlers [logHandler]

    debugM "Main.main" $ "Enter"

    options <- cmdArgs options

    runClient (name_ options) $ do
        connectClient (host options) (port options)
        runOnTimer timeout $ do
            liftIO getTime >>= updateClient
            return True

    debugM "Main.main" $ "Exit"

    where
        timeout = 1

        getTime :: IO String
        getTime = formatTime defaultTimeLocale "%a %d %b %Y %T %Z" <$> getZonedTime

