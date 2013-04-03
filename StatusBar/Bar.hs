module StatusBar.Request where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TMVar
import Control.Monad
import Control.Monad.STM

import Network.Socket

import System.IO
import System.IO.Unsafe
import System.Process

import StatusBar.Process
import StatusBar.Request

-- | A widget on the bar
data Widget = Widget String Int String

requestChan :: TChan Request
requestChan = unsafePerformIO $ newTChanIO

dzenChan :: TChan String
dzenChan = unsafePerformIO $ newTChanIO

widgetListVar :: TMVar [Widget]
widgetListVar = unsafePerformIO $ newTMVarIO []

barStartup :: Int -> IO ()
barStartup n = do
    forkIO . startDzen
    replicateM_ n . forkIO $ processBarRequest

barRequestHandler :: Request -> IO ()
barRequestHandler request = writeTChan requestChan request

