{-# OPTIONS_GHC -XDeriveDataTypeable #-}
module Main (main) where

import Control.Applicative
import Control.Monad

import System.Console.CmdArgs
import System.Environment
import System.IO
import System.Log.Logger
import System.Log.Handler.Simple

import BarTender.Server
import BarTender.Bar

data Options = Options
    { handlers :: Int
    , host     :: String
    , port     :: String
    , queue    :: Int
    , timeout  :: Int
    }
    deriving (Show, Data, Typeable)

options :: Options
options = Options
    { handlers = 3
        &= help "The number of message handlers for the bar"
    , host = def &= argPos 0 &= opt "localhost"
    , port = "9999"
        &= help "The server port"
    , queue = 5
        &= help "The number of queued packets to allow"
    , timeout = 300
        &= help "The time to wait before killing a widget"
    }
    &= program "StatusBar"
    &= summary "StatusBar v0.1.0"
    &= help "Display a status bar that clients can connect to"

main :: IO ()
main = do
    logHandler <- verboseStreamHandler stderr DEBUG
    updateGlobalLogger rootLoggerName $ setLevel DEBUG . setHandlers [logHandler]

    debugM "Main.main" $ "Enter"

    options <- cmdArgs options
    debugM "Main.main" $ "Options: " ++ show options

    -- TODO: Use the dzen arguments here
    barStartup (handlers options) (timeout options)
    serveBar (Just $ host options) (Just $ port options) (queue options) barMessageHandler

    debugM "Main.main" $ "Exit"

