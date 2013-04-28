import Control.Applicative
import Control.Monad

import System.Environment
import System.IO
import System.Log.Logger
import System.Log.Handler.Simple

import StatusBar.Client
import StatusBar.Dzen

main :: IO ()
main = do
    logHandler <- verboseStreamHandler stderr DEBUG
    updateGlobalLogger rootLoggerName $ setLevel DEBUG . setHandlers [logHandler]
    (hostname : port : remArgs) <- getArgs

    connectClient


