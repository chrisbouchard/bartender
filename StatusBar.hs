import Control.Applicative
import Control.Monad

import System.Environment
import System.IO
import System.Log.Logger
import System.Log.Handler.Simple

import StatusBar.Server
import StatusBar.Bar

main :: IO ()
main = do
    logHandler <- verboseStreamHandler stderr DEBUG
    updateGlobalLogger rootLoggerName $ setLevel DEBUG . setHandlers [logHandler]
    (hostname : port : remArgs) <- getArgs
    barStartup nMessHandlers timeout remArgs
    serveBar (Just hostname) (Just port) nQueued barMessageHandler
    where
        nQueued = 5
        nMessHandlers = 3
        timeout = 20
