import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM.TMVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM

import Data.Time.Format
import Data.Time.LocalTime

import System.Environment
import System.IO
import System.Locale
import System.Log.Logger
import System.Log.Handler.Simple

import StatusBar.Client
import StatusBar.Dzen
import StatusBar.Timer

main :: IO ()
main = do
    logHandler <- verboseStreamHandler stderr DEBUG
    updateGlobalLogger rootLoggerName $ setLevel DEBUG . setHandlers [logHandler]
    (hostname : port : remArgs) <- getArgs

    runClient name $ do
        connectClient hostname port
        runOnTimer timeout $ do
            liftIO getTime >>= updateClient
            return True

    where
        name = "Test"
        timeout = 1

        getTime :: IO String
        getTime = formatTime defaultTimeLocale "%a %d %b %Y %T %Z" <$> getZonedTime

