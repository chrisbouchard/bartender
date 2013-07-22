module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import GHC.IO (evaluate)

import System.Console.CmdArgs
import System.Environment
import System.Exit
import System.IO
import System.Log.Logger
import System.Log.Handler.Simple
import System.Process

import BarTender.Client
import BarTender.Dzen
import BarTender.Timer

-- Options to this program, gotten from the command line, a config file, or
-- something like that
data Options = Options
    { delay   :: Int    -- The time between updates (in seconds)
    , errexit :: Bool   -- Exit if the command has non-zero exit
    , host    :: String -- The server host
    , name_   :: String -- The client name
    , port    :: Int    -- The server port
    , retries :: Int    -- The number of times to retry connecting
    , timeout :: Int    -- The number of seconds to wait for server response
    , command :: String -- The command to run
    }
    deriving (Show, Data, Typeable)

-- Set default options and annotations
defaultOptions :: Options
defaultOptions = Options
    { delay = 10
        &= help "The time between updates"
    , errexit = False
        &= help "Exit if the command has non-zero exit"
    , name_ = "Test"
        &= help "Specify a client name"
    , port = 9999
        &= help "The server port"
    , retries = 0
        &= help "Number of times to retry connecting"
    , timeout = 30
        &= help "Number of seconds to wait for server response"
    , host = def &= argPos 0
    , command = def &= argPos 1
    }
    &= program "WatchClient"
    &= summary "WatchClient v0.1.0"
    &= help "Watch a command and send its output to a StatusBar server."

connectionOptions :: Options -> ConnectionOptions
connectionOptions options = ConnectionOptions
    { connectHost    = host options
    , connectPort    = show $ port options
    , connectRetries = retries options
    , connectTimeout = timeout options
    }

main :: IO ()
main = do
    -- Set up logging
    logHandler <- verboseStreamHandler stderr DEBUG
    updateGlobalLogger rootLoggerName $ setLevel DEBUG . setHandlers [logHandler]

    debugM "Main.main" $ "Enter"

    options <- cmdArgs defaultOptions

    runClient (name_ options) $ do
        connectClient $ connectionOptions options
        updateFromCommand options
        runOnTimer (delay options) $ updateFromCommand options

    debugM "Main.main" $ "Exit"

updateFromCommand :: MonadIO m => Options -> BarClient m Bool
updateFromCommand options = do
    liftIO . debugM "Main.updateFromCommand" $ "Enter"
    liftIO . debugM "Main.updateFromCommand" $ "Running: " ++ command options
    (cmdIn, cmdOut, _, cmdHandle) <- liftIO . runInteractiveCommand $
        command options
    liftIO . debugM "Main.updateFromCommand" $ "Closing process's stdin"
    -- liftIO $ hClose cmdIn
    liftIO . debugM "Main.updateFromCommand" $ "Getting output"
    output <- liftIO $ hGetContents cmdOut >>= evaluate
    liftIO . debugM "Main.updateFromCommand" $ "Output: " ++ output
    liftIO . debugM "Main.updateFromCommand" $ "Sending output to server"
    updateClient output
    liftIO . debugM "Main.updateFromCommand" $ "Waiting for exit status"
    code <- liftIO $ waitForProcess cmdHandle
    value <- return $ case code of
        ExitSuccess   -> True
        ExitFailure _ -> not $ errexit options
    liftIO . debugM "Main.updateFromCommand" $ "Exit with: " ++ show value
    return value

