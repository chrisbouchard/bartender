module Main (main) where

import Prelude hiding ((.), id)

import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.IO.Class

import Data.Lens.Common
import Data.Lens.Template

import GHC.IO (evaluate)

import System.Environment
import System.Exit
import System.IO
import System.Log.Logger
import System.Log.Handler.Simple
import System.Process

import BarTender.Client
import BarTender.Dzen
import BarTender.Options
import BarTender.Timer
import BarTender.Util

-- Options to this program, gotten from the command line, a config file, or
-- something like that
data Options = Options
    { _delay    :: Int               -- The time between updates (in seconds)
    , _errexit  :: Bool              -- Exit if the command has non-zero exit
    , _name     :: String            -- The client name
    , _command  :: String            -- The command to run
    , _help     :: Bool              -- Whether the user wants the help message
    , _connOpts :: ConnectionOptions -- The client connection options
    }
    deriving Show

$( makeLenses [ ''Options ] )

defaultOptions :: Options
defaultOptions = Options
    { _name = "WatchClient"
    , _delay = 10
    , _errexit = False
    , _command = ""
    , _help = False
    , _connOpts = defaultConnectionOptions
    }

optionDescriptions :: [OptDescr (Options -> Either String Options)]
optionDescriptions = (++)
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
    , Option ['d'] ["delay"]
        (flip ReqArg "N" $ \str -> case maybeRead str of
            Just n  -> Right . (delay ^= n)
            Nothing -> const . Left $ "Invalid number '" ++ str ++ "'")
        "Number of seconds to wait between updates"
    , Option ['h', '?'] ["help"]
        (NoArg $ Right . (help ^= True))
        "Display this help message"
    ] $
    [ Option ['e'] ["errexit"]
        (NoArg $ \b -> Right . (errexit ^= b))
        "Exit if the command has non-zero exit"
    ] >>= completeOption

main :: IO ()
main = do
    -- Set up logging
    logHandler <- verboseStreamHandler stderr DEBUG
    updateGlobalLogger rootLoggerName $ setLevel DEBUG . setHandlers [logHandler]

    debugM "Main.main" $ "Enter"

    errorOrOptions <- handleOpt (Exactly 2) defaultOptions <$>
        getOpt RequireOrder optionDescriptions <$> getArgs

    case errorOrOptions of
        Left error -> putStrLn error >> putStrLn "" >> printHelpMessage
        Right (opts, posArgs) -> let options = handlePositional posArgs opts in
            if options ^. help
                then printHelpMessage
                else runClient (options ^. name) $ do
                    connectClient $ options ^. connOpts
                    updateFromCommand options
                    runOnTimer (options ^. delay) $ updateFromCommand options

    debugM "Main.main" $ "Exit"
    where
        handlePositional :: [String] -> Options -> Options
        handlePositional posArgs = foldr (.) id
            [ connectHost . connOpts ^= posArgs !! 0
            , command ^= posArgs !! 1
            ]

        printHelpMessage :: IO ()
        printHelpMessage = do
            putStr $ usageInfo "WatchClient" optionDescriptions

        updateFromCommand :: MonadIO m => Options -> BarClient m Bool
        updateFromCommand options = do
            liftIO . debugM "Main.updateFromCommand" $ "Enter"
            liftIO . debugM "Main.updateFromCommand" $ "Running: " ++ (options ^. command)
            (cmdIn, cmdOut, _, cmdHandle) <- liftIO . runInteractiveCommand $
                options ^. command
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
                ExitFailure _ -> not $ options ^. errexit
            liftIO . debugM "Main.updateFromCommand" $ "Exit with: " ++ show value
            return value

