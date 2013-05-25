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
import System.Process

import StatusBar.Client
import StatusBar.Dzen
import StatusBar.Timer

-- Options to this program, gotten from the command line, a config file, or
-- something like that
data Options = Options
    { delay   :: Int    -- The time between updates (in seconds)
    , errexit :: Bool   -- Exit if the command has non-zero exit
    , host    :: String -- The server host
    , name_   :: String -- The client name
    , port    :: Int    -- The server port
    , command :: String -- The command to run
    }
    deriving (Show, Data, Typeable)

-- Set default options and annotations
options :: Options
options = Options
    { delay = 10
        &= help "The time between updates"
    , errexit = False
        &= help "Exit if the command has non-zero exit"
    , name_ = "Test"
        &= help "Specify a client name"
    , port = 9999
        &= help "The server port"
    , host = def &= argPos 0
    , command = def &= argPos 1
    }
    &= program "WatchClient"
    &= summary "WatchClient v0.1.0"
    &= help "Watch a command and send its output to a StatusBar server."

main :: IO ()
main = do
    -- Set up logging
    logHandler <- verboseStreamHandler stderr DEBUG
    updateGlobalLogger rootLoggerName $ setLevel DEBUG . setHandlers [logHandler]

    debugM "Main.main" $ "Enter"

    options <- cmdArgs options

    runClient (name_ options) $ do
        connectClient (host options) (show $ port options)
        runOnTimer (delay options) $ do
            (_, cmdOut, _, cmdHandle) <- liftIO . runInteractiveCommand $ command options
            liftIO (hGetContents cmdOut) >>= updateClient
            code <- liftIO $ waitForProcess cmdHandle
            return $ case code of
                ExitSuccess   -> True
                ExitFailure _ -> not $ errexit options

    debugM "Main.main" $ "Exit"

