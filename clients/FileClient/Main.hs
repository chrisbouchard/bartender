module Main (main) where

import Prelude hiding ((.), id)

import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.IO.Class

import Data.Lens.Common
import Data.Lens.Template

import System.Environment
import System.IO
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
    , _path     :: FilePath          -- The command to run
    , _connOpts :: ConnectionOptions -- The client connection options
    }
    deriving Show

$( makeLenses [ ''Options ] )

defaultOptions :: Options
defaultOptions = Options
    { _name = "FileClient"
    , _help = False
    , _path = "-"
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

    errorOrOptions <- handleOpt (Between 1 2) defaultOptions <$>
        getOpt RequireOrder optionDescriptions <$> getArgs

    debugM "Main.main" $ "errorOrOptions: " ++ show errorOrOptions

    case errorOrOptions of
        Left error -> putStrLn error >> putStrLn "" >> printHelpMessage
        Right (opts, posArgs) -> let options = handlePositional posArgs opts in
            if options ^. help
                then printHelpMessage
            else void $ do
                handle <- openInputHandle $ options ^. path
                hSetBuffering handle LineBuffering

                runClient (options ^. name) $ do
                    connectClient $ options ^. connOpts
                    doWhile not $ do
                        liftIO (hGetLine handle) >>= updateClient
                        liftIO $ hIsEOF handle

    debugM "Main.main" $ "Exit"
    where
        openInputHandle :: FilePath -> IO Handle
        openInputHandle path = if path == "-"
            then return stdin
            else openFile path ReadMode

        handlePositional :: [String] -> Options -> Options
        handlePositional posArgs = foldr (.) id
            [ connectHost . connOpts ^= posArgs !! 0
            , if length posArgs == 2 then (path ^= posArgs !! 1) else id
            ]

        printHelpMessage :: IO ()
        printHelpMessage = do
            putStr $ usageInfo "FileClient" optionDescriptions

