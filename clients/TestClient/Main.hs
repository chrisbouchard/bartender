module Main (main) where

import Prelude hiding ((.), id)

import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.IO.Class

import Data.Lens.Common
import Data.Lens.Template
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
    { _name     :: String            -- The client name
    , _help     :: Bool              -- Whether the user wants the help message
    , _connOpts :: ConnectionOptions -- The client connection options
    }
    deriving Show

$( makeLenses [ ''Options ] )

defaultOptions :: Options
defaultOptions = Options
    { _name = "TestClient"
    , _help = False
    , _connOpts = defaultConnectionOptions
    }

optionDescriptions :: [OptDescr (Options -> Either String Options)]
optionDescriptions =
    [ Option ['p'] ["port"]
        (flip ReqArg "PORT" $ \str -> Right . (connectPort . connOpts ^= str))
        "The server port"
    , Option ['r'] ["retries"]
        (flip ReqArg "N" $ \str -> case maybeRead str of
            Just n  -> Right . (connectRetries . connOpts ^= n)
            Nothing -> const . Left $ "Invalid number '" ++ str ++ "'")
        "Number of times to retry connecting"
    , Option ['t'] ["timeout"]
        (flip ReqArg "N" $ \str -> case maybeRead str of
            Just n  -> Right . (connectTimeout . connOpts ^= n)
            Nothing -> const . Left $ "Invalid number '" ++ str ++ "'")
        "Number of seconds to wait for server response"
    , Option ['n'] ["name"]
        (flip ReqArg "NAME" $ \str -> Right . (name ^= str))
        "The name the client reports to the server"
    , Option ['h', '?'] ["help"]
        (NoArg $ Right . (help ^= True))
        "Display this help message"
    ]

main :: IO ()
main = do
    -- Set up logging
    logHandler <- verboseStreamHandler stderr DEBUG
    updateGlobalLogger rootLoggerName $ setLevel DEBUG . setHandlers [logHandler]

    debugM "Main.main" $ "Enter"

    errorOrOptions <- handleOpt (AtMost 1) defaultOptions <$>
        getOpt RequireOrder optionDescriptions <$> getArgs

    case errorOrOptions of
        Left error -> putStrLn error >> putStrLn "" >> printHelpMessage
        Right (opts, posArgs) -> let options = handlePositional posArgs opts in
            if options ^. help
                then printHelpMessage
                else runClient (options ^. name) $ do
                    connectClient $ options ^. connOpts
                    runOnTimer delay $ do
                        liftIO getTime >>= updateClient
                        return True

    debugM "Main.main" $ "Exit"

    where
        delay = 1

        getTime :: IO String
        getTime = formatTime defaultTimeLocale "%a %d %b %Y %R %Z" <$> getZonedTime

        handlePositional :: [String] -> Options -> Options
        handlePositional posArgs = (connectHost . connOpts ^= posArgs !! 0)

        printHelpMessage :: IO ()
        printHelpMessage = do
            putStr $ usageInfo "TestClient" optionDescriptions

