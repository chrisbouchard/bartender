module StatusBar.Bar
    ( barStartup
    , barRequestHandler
    , protocolVersion
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TMVar
import Control.Monad
import Control.Monad.STM

import Data.List
import Data.Maybe

import Network.Socket

import System.IO
import System.IO.Unsafe
import System.Log.Logger
import System.Log.Handler.Simple
import System.Process

import StatusBar.Dzen
import StatusBar.Process
import StatusBar.Request
import StatusBar.Timer
import StatusBar.Widget

-- The channel to send messages to message-processors
messageChan :: TChan (Message, TChan (Maybe Message))
messageChan = unsafePerformIO $ newTChanIO

-- The list of widgets on the status bar
widgetListVar :: TMVar [Widget]
widgetListVar = unsafePerformIO $ newTMVarIO []

-- | The version of this protocol that we implement
protocolVersion = 1

-- | Start and initialize the status bar
barStartup :: Int -- ^ The number of message handlers to fork
           -> Int -- ^ The timeout for widget timers
           -> IO ()
barStartup n timeout = do
    debugM "StatusBar.Bar.barStartup" $ "Enter"
    (dzenStdin, dzenProc) <- startDzen
    forkChildFinally (feedDzen dzenStdin) $ \_ -> do
        debugM "StatusBar.Bar.barStartup" $ "Child thread has exited"
        debugM "StatusBar.Bar.barStartup" $ "Closing dzen stdin: " ++ show dzenStdin
        hClose dzenStdin
        debugM "StatusBar.Bar.barStartup" $ "Terminating dzen process"
        terminateProcess dzenProc
    startTimer cleanupDelay cleanupTimerHandler
    replicateM_ n . forkChild $ processBarMessages
    debugM "StatusBar.Bar.barStartup" $ "Started " ++ show n ++ " message processors."
    debugM "StatusBar.Bar.barStartup" $ "Exit"
    where
        -- Start the dzen process and grab its standard input
        startDzen :: IO (Handle, ProcessHandle)
        startDzen = do
            debugM "StatusBar.Bar.startDzen" $ "Enter"
            (dzenStdin, _, _, dzenProc) <- runInteractiveProcess "dzen2"
                [ "-p"
                , "-h", "16"
                , "-bg", "black"
                , "-fg", "white"
                , "-ta", "l"
                ]
                -- [ "-geometry", "+0-0"
                -- , "-p"
                -- , "-h", "16"
                -- , "-bg", "black"
                -- , "-fg", "white"
                -- , "-ta", "l"
                -- ]
                Nothing -- Don't specify a PWD
                Nothing -- Don't specify an environment
            debugM "StatusBar.Bar.startDzen" $ "Started dzen"
            debugM "StatusBar.Bar.startDzen" $ "    stdin: " ++ show dzenStdin
            debugM "StatusBar.Bar.startDzen" $ "Exit"
            return $ (dzenStdin, dzenProc)

        -- Send status updates to dzen
        feedDzen :: Handle -> IO ()
        feedDzen dzenStdin = forever $ do
            status <- atomically $ do
                widgetList <- readTMVar widgetListVar
                msum $ map (takeTMVar . wLock) widgetList
                getStatus
            debugM "StatusBar.Bar.feedDzen" $ "Read a new status line"
            debugM "StatusBar.Bar.feedDzen" $ "    status: " ++ status
            hPutStrLn dzenStdin status
            hFlush dzenStdin
            debugM "StatusBar.Bar.feedDzen" $ "Wrote status"

        cleanupDelay :: Int
        cleanupDelay = 60

        cleanupTimerHandler :: TimerId -> IO ()
        cleanupTimerHandler tid = do
            debugM "StatusBar.Bar.cleanupTimerHandler" $ "Enter"
            (before, after) <- atomically $ do
                widgetList <- takeTMVar widgetListVar
                newWidgetList <- filterM (liftM not . widgetIsDead) widgetList
                putTMVar widgetListVar newWidgetList
                return (length widgetList, length newWidgetList)
            debugM "StatusBar.Bar.cleanupTimerHandler" $ "Before: " ++ show before
            debugM "StatusBar.Bar.cleanupTimerHandler" $ "After: " ++ show after
            startTimer cleanupDelay cleanupTimerHandler
            debugM "StatusBar.Bar.cleanupTimerHandler" $ "Exit"

        getStatus :: STM String
        getStatus = do
            widgetList <- readTMVar widgetListVar
            intercalate statusDelim . catMaybes <$> mapM widgetStatus widgetList
            where
                statusDelim = dzenColorFG disabledColor " | "

        -- Listen for messages on the channel, and update the bar for each
        processBarMessages :: IO ()
        processBarMessages = do
            debugM "StatusBar.Bar.processBarMessages" $ "Enter"
            forever $ do
                (message, chan) <- atomically $ readTChan messageChan
                debugM "StatusBar.Bar.processBarMessages" $ "Received: " ++ show message
                mResponse <- updateBar message
                case mResponse of
                    (Just response) -> debugM "StatusBar.Bar.processBarMessages" $
                        "Response: " ++ show response
                    Nothing         -> return ()
                atomically $ writeTChan chan mResponse
            warningM "StatusBar.Bar.processBarMessages" $ "This IO operation should not terminate."
            debugM "StatusBar.Bar.processBarMessages" $ "Exit"

        -- Take a message and update the status bar appropriately
        updateBar :: Message -> IO (Maybe Message)

        updateBar (RInit name) = do
            debugM "StatusBar.Bar.updateBar" $ "Enter RInit"
            widget <- newWidget name timeout
            debugM "StatusBar.Bar.updateBar" $ "widget: " ++ show widget
            infoM "StatusBar.Bar.updateBar" $
                "Added widget " ++ name ++ " with ID " ++ show (wId widget)
            atomically $ do
                widgetList <- takeTMVar widgetListVar
                putTMVar widgetListVar $ insert widget widgetList
            debugM "StatusBar.Bar.updateBar" $ "Exit"
            return . Just $ RAck (wId widget) timeout protocolVersion

        updateBar (RUpdate cid content) = do
            debugM "StatusBar.Bar.updateBar" $ "Enter RUpdate"
            mWidget <- atomically $ do
                widgetList <- readTMVar widgetListVar
                return $ find (widgetHasId cid) widgetList
            debugM "StatusBar.Bar.updateBar" $ "mWidget: " ++ show mWidget
            case mWidget of
                (Just widget) -> do
                    updateWidget widget $ Just content
                    debugM "StatusBar.Bar.updateBar" $ "Updated widget"
                    debugM "StatusBar.Bar.updateBar" $ "    content: " ++ content
                    debugM "StatusBar.Bar.updateBar" $ "Exit"
                    return Nothing
                Nothing       -> do
                    warningM "StatusBar.Bar.updateBar" $
                        "Request for non-existent widget ID " ++ show cid
                    debugM "StatusBar.Bar.updateBar" $ "Exit"
                    return . Just $ badClientMessage cid

        updateBar (RAlive cid) = do
            debugM "StatusBar.Bar.updateBar" $ "Enter RAlive"
            mWidget <- atomically $ do
                widgetList <- readTMVar widgetListVar
                return $ find (widgetHasId cid) widgetList
            debugM "StatusBar.Bar.updateBar" $ "mWidget: " ++ show mWidget
            case mWidget of
                (Just widget) -> do
                    updateWidget widget Nothing
                    debugM "StatusBar.Bar.updateBar" $ "Touched widget"
                    debugM "StatusBar.Bar.updateBar" $ "Exit"
                    return Nothing
                Nothing       -> do
                    warningM "StatusBar.Bar.updateBar" $
                        "Request for non-existent widget ID " ++ show cid
                    debugM "StatusBar.Bar.updateBar" $ "Exit"
                    return . Just $ badClientMessage cid

        updateBar message = do
            debugM "StatusBar.Bar.updateBar" $ "Enter other"
            return . Just $ notImplementedMessage message

-- | Request handler for a status bar server that updates the status bar
barRequestHandler :: RequestHandler
barRequestHandler (Request client message) = do
    debugM "StatusBar.Bar.barRequestHandler" $ "Enter"
    chan <- newTChanIO
    debugM "StatusBar.Bar.barRequestHandler" $ "Created channel"
    atomically $ writeTChan messageChan (message, chan)
    debugM "StatusBar.Bar.barRequestHandler" $ "Wrote into the message channel"
    -- One <$> to lift to IO a, then another to lift to IO (Maybe a)
    mResponse <- atomically $ (Request client <$>) <$> readTChan chan
    debugM "StatusBar.Bar.barRequestHandler" $ "Got a response back"
    debugM "StatusBar.Bar.barRequestHandler" $ "    mResponse: " ++ show mResponse
    debugM "StatusBar.Bar.barRequestHandler" $ "Exit"
    return mResponse

