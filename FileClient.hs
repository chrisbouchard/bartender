{-# OPTIONS_GHC -XDeriveDataTypeable #-}
module Main (main) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import Data.Time.Format
import Data.Time.LocalTime

import System.Console.CmdArgs
import System.Exit
import System.IO
import System.Log.Logger
import System.Log.Handler.Simple
import System.Process

import BarTender.Client
import BarTender.Dzen
import BarTender.Timer
import BarTender.Util

-- Options to this program, gotten from the command line, a config file, or
-- something like that
data Options = Options
    { host  :: String -- The server host
    , name_ :: String -- The client name
    , port  :: Int    -- The server port
    , path  :: String -- The command to run
    }
    deriving (Show, Data, Typeable)

-- Set default options and annotations
options :: Options
options = Options
    { name_ = "Test"
        &= help "Specify a client name"
    , port = 9999
        &= help "The server port"
    , host = def &= argPos 0
    , path = def &= opt "-" &= argPos 1
    }
    &= program "FileClient"
    &= summary "FileClient v0.1.0"
    &= help "Read lines from a file and send them to a StatusBar server."

main :: IO ()
main = do
    -- Set up logging
    logHandler <- verboseStreamHandler stderr DEBUG
    updateGlobalLogger rootLoggerName $ setLevel DEBUG . setHandlers [logHandler]

    debugM "Main.main" $ "Enter"

    options <- cmdArgs options

    debugM "Main.main" $ "Options: " ++ show options

    handle <- openInputHandle $ path options
    hSetBuffering handle LineBuffering

    runClient (name_ options) $ do
        connectClient (host options) (show $ port options)
        doWhile not $ do
            liftIO (hGetLine handle) >>= updateClient
            liftIO $ hIsEOF handle

    debugM "Main.main" $ "Exit"
    where
        openInputHandle :: String -> IO Handle
        openInputHandle path = if path == "-"
            then return stdin
            else openFile path ReadMode

