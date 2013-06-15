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
    { handlers   :: Int
    , host       :: String
    , port       :: Int
    , bufferSize :: Int
    , timeout    :: Int
    }
    deriving (Show, Data, Typeable)

options :: Options
options = Options
    { handlers = 3
        &= help "The number of message handlers for the bar"
    , host = def &= argPos 0 &= opt "localhost"
    , port = 9999
        &= help "The server port"
    , bufferSize = 1024
        &= help "The buffer size for incoming packets"
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


    barStartup $ barOptions options
    serveBar (serverOptions options) barMessageHandler

    debugM "Main.main" $ "Exit"
    where
        serverOptions options = defaultServerOptions
            { serverHost = host options
            , serverPort = show $ port options
            , serverBufferSize = bufferSize options
            }

        barOptions options = defaultBarOptions
            { barMessageHandlers = handlers options
            , barTimeout         = timeout options
            }

