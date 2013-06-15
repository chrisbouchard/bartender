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

import BarTender.Client
import BarTender.Dzen
import BarTender.Timer

-- Options to this program, gotten from the command line, a config file, or
-- something like that
data Options = Options
    { host    :: String -- The server host
    , port    :: Int    -- The server port
    , name_   :: String -- The client name
    , retries :: Int    -- The number of times to retry connecting
    , timeout :: Int    -- The number of seconds to wait for server response
    }
    deriving (Show, Data, Typeable)

-- Set default options and annotations
options :: Options
options = Options
    { port = 9999
        &= help "The server port"
    , name_ = "Test"
        &= help "Specify a client name"
    , retries = 0
        &= help "Number of times to retry connecting"
    , timeout = 30
        &= help "Number of seconds to wait for server response"
    , host = def &= argPos 0 &= opt "localhost"
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
        connectClient $ connectionOptions options
        runOnTimer delay $ do
            liftIO getTime >>= updateClient
            return True

    debugM "Main.main" $ "Exit"

    where
        delay = 1

        getTime :: IO String
        getTime = formatTime defaultTimeLocale "%a %d %b %Y %R %Z" <$> getZonedTime

        connectionOptions :: Options -> ConnectionOptions
        connectionOptions options = ConnectionOptions
            { connectHost    = host options
            , connectPort    = show $ port options
            , connectRetries = retries options
            , connectTimeout = timeout options
            }

