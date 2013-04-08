module StatusBar.Timer
    ( Timer
    , newTimer
    , killTimer
    , restartTimer
    , startTimer
    , stopTimer
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.STM

import Data.Maybe

import System.Log.Logger
import System.Log.Handler.Simple

-- | A timer that asynchronously runs an action every so often.
data Timer = Timer (TChan TimerMessage)

data TimerMessage = TChild ThreadId
                  | TKill
                  | TRestart
                  | TStart
                  | TStop
    deriving (Eq, Show)

type TimerStateT a = StateT (Maybe ThreadId) IO a

-- | Create a new timer in a stopped state.
newTimer :: Int      -- ^ The timeout
         -> IO Bool  -- ^ The action to perform. The return value indicates
                     -- whether the timer should continue or stop.
         -> IO Timer
newTimer timeout action = do
    chan <- newTChanIO
    forkIO $ timerLoop chan
    return $ Timer chan
    where
        -- Convert the timeout to microseconds because that's what threadDelay
        -- uses.
        timeoutMicro = timeout * 1000000

        -- Listen for messages on the message channel from the child thread or
        -- from the outside world.
        timerLoop :: TChan TimerMessage -> IO ()
        timerLoop chan = void $ flip runStateT Nothing $ forever $ do
            message <- liftIO . atomically $ readTChan chan
            myTid <- liftIO $ myThreadId
            liftIO . debugM "StatusBar.Timer.timerLoop" $
                "(" ++ show myTid ++ ") Received message: " ++ show message
            handleMessage chan message

        -- Handle messages that the timer receives from its child and from the
        -- outside world.
        handleMessage :: TChan TimerMessage -> TimerMessage -> TimerStateT ()

        -- Handle the child proccess exiting
        -- This means it's time to run the action, so do that then check if we
        -- should start again.
        handleMessage chan (TChild tid) = do
            mChildTid <- get
            when (fromMaybe False $ liftM (==tid) mChildTid) $ do
                put Nothing
                continue <- liftIO action
                when continue $ handleMessage chan TStart

        -- Handle a request to kill the timer
        -- Close up shop: Kill the child thread, then kill our own thread.
        handleMessage chan TKill = do
            handleMessage chan TStop
            liftIO $ myThreadId >>= killThread

        -- Handle a request to restart the timer
        -- First stop the timer, then start it again. We do this using
        -- handleMessage directly instead of using the message channel so that
        -- these run atomically.
        handleMessage chan TRestart = do
            handleMessage chan TStop
            handleMessage chan TStart

        -- Handle a request to start the timer.
        -- If the timer is already running, this is a no-op. Otherwise, start
        -- up a child thread.
        handleMessage chan TStart = do
            mChildTid <- get
            when (isNothing mChildTid) $ do
                (liftIO . forkIO) (timerChild chan) >>= put . Just

        -- Handle a request to stop the timer.
        -- Kill the child thread if there is one.
        handleMessage chan TStop = do
            get >>= liftIO . fromMaybe (return ()) . liftM killThread
            put Nothing

        -- Simple IO to simply wait a specified amount of time then announce
        -- success to the parent thread
        timerChild :: TChan TimerMessage -> IO ()
        timerChild chan = do
            myTid <- myThreadId
            debugM "StatusBar.Timer.timerChild" $ "(" ++ show myTid ++ ") Enter"
            threadDelay timeoutMicro
            debugM "StatusBar.Timer.timerChild" $ "(" ++ show myTid ++ ") Writing to parent"
            atomically . writeTChan chan $ TChild myTid
            debugM "StatusBar.Timer.timerChild" $ "(" ++ show myTid ++ ") Exit"

-- | Stop the timer and destroy it. After this, the timer cannot be restarted
-- and calling functions on the timer may block.
killTimer :: Timer -> IO ()
killTimer (Timer chan) = atomically $ writeTChan chan TKill

-- | Stop the timer and start it again.
restartTimer :: Timer -> IO ()
restartTimer (Timer chan) = atomically $ writeTChan chan TRestart

-- | Start the timer. After the timeout specified when the timer was created,
-- the timer's action will be run. Starting a running timer is a no-op.
startTimer :: Timer -> IO ()
startTimer (Timer chan) = atomically $ writeTChan chan TStart

-- | Stop the timer if it is running. Stopping a stopped timer is a no-op.
stopTimer :: Timer -> IO ()
stopTimer (Timer chan) = atomically $ writeTChan chan TStop

