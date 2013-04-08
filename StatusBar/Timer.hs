module StatusBar.Timer where

import Control.Concurrent

import System.Log.Logger
import System.Log.Handler.Simple

type TimerId = ThreadId

startTimer :: Int -> (TimerId -> IO ()) -> IO (TimerId)
startTimer timeout action = forkIO $ do
    -- Convert from seconds to microseconds
    threadDelay $ timeout * 1000000
    myThreadId >>= action

stopTimer :: TimerId -> IO ()
stopTimer = killThread

