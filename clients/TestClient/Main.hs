module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Data.Time.Format
import Data.Time.LocalTime

import System.Environment
import System.Exit
import System.IO
import System.Locale
import System.Log.Logger
import System.Log.Handler.Simple

import BarTender.Client
import BarTender.Dzen
import BarTender.Options
import BarTender.Timer
import BarTender.Util

-- Options to this program, gotten from the command line, a config file, or
-- something like that
data Options = Options
    { name     :: String            -- The client name
      help     :: Bool              -- Whether the user wants the help message
      connOpts :: ConnectionOptions -- The client connection options
    }
    deriving (Show)

defaultOptions :: Options
defaultOptions = Options
    { name = "Test"
      help = False
    , connOpts = defaultConnectionOptions
    }

-- Set default options and annotations
-- options :: Options
-- options = Options
--     { port = 9999
--         &= help "The server port"
--     , name_ = "Test"
--         &= help "Specify a client name"
--     , retries = 0
--         &= help "Number of times to retry connecting"
--     , timeout = 30
--         &= help "Number of seconds to wait for server response"
--     , host = def &= argPos 0 &= opt "localhost"
--     }
--     &= program "TestClient"
--     &= summary "TestClient v0.1.0"
--     &= help "A test client to connect to the StatusBar server"

optionDescriptions :: [OptDescr (Options -> Options)
optionDescriptions =
    [ Option ['p'] ["port"]
        (ReqArg $ \str opts@(Options {connOpts = cOpts}) ->
            opts { connOpts = cOpts { connectPort = str } }
        "The server port"
    , Option ['r'] ["retries"]
        (ReqArg $ \str opts@(Options {connOpts = cOpts}) ->
            opts { connOpts = cOpts { connectRetries = str } }
        "Number of times to retry connecting"
    , Option ['t'] ["timeout"]
        (ReqArg $ \str opts@(Options {connOpts = cOpts}) ->
            case maybeRead str of
                Just n  -> Right $ opts { connOpts = cOpts { connectTimeout = n } }
                Nothing -> Left $ "Invalid timeout '" ++ str ++ "'"
        "Number of seconds to wait for server response"
    , Option ['h', '?'] ["help"]
        (NoArg $ \opts -> opts { help = True })
        "Display this help message"
    ]

main :: IO ()
main = do
    -- Set up logging
    logHandler <- verboseStreamHandler stderr DEBUG
    updateGlobalLogger rootLoggerName $ setLevel DEBUG . setHandlers [logHandler]

    debugM "Main.main" $ "Enter"

    errorOrOptions <- cmdArgs options

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

