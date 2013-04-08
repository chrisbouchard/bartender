module StatusBar.Widget
    ( Widget, wName, wId, wLock
    , LockVar
    , newWidget
    , updateWidget
    , widgetHasId
    , widgetIsDead
    , widgetStatus
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM.TMVar
import Control.Monad
import Control.Monad.STM

import Data.Maybe

import System.IO.Unsafe
import System.Log.Logger
import System.Log.Handler.Simple

import StatusBar.Dzen
import StatusBar.Timer

-- | A lock on the widget. When a widget is updated, it will be unlocked. Once
-- the widget is displayed it should be relocked.
type LockVar = TMVar ()

-- | A widget on the bar
data Widget = Widget
    { wName  :: String
    , wId    :: Int
    , wTimer :: Timer
    , wState :: TMVar WidgetState
    , wLock  :: LockVar
    }

instance Eq Widget where
    (Widget _ lWid _ _ _) == (Widget _ rWid _ _ _) = lWid == rWid

instance Ord Widget where
    compare (Widget lName lWid _ _ _) (Widget rName rWid _ _ _) =
        compare (lName, lWid) (rName, rWid)

instance Show Widget where
    show (Widget name wid _ _ _) = "(Widget " ++ name ++ " " ++ show wid ++ ")"

-- | The state of a widget
data WidgetState = WidgetState
    { wsContent :: Maybe String
    , wsDeath   :: WidgetDeath
    }

-- | How close to death a widget is
data WidgetDeath = WDAlive
                 | WDDying
                 | WDDead
    deriving (Show, Eq)

-- The next valid widget id. This is only ever increased, so old ids that
-- become invalid will not be reused.
nextIdVar :: TMVar Int
nextIdVar = unsafePerformIO $ newTMVarIO 0

-- | Create a new widget with the given name
newWidget :: String -- ^ The widget's name
          -> Int -- ^ The timeout for the widget's timer
          -> IO Widget
newWidget name timeout = do
    debugM "StatusBar.Widget.newWidget" $ "Enter"
    wid <- atomically $ do
        nextId <- takeTMVar nextIdVar
        putTMVar nextIdVar (nextId + 1)
        return nextId
    stateVar <- newEmptyTMVarIO
    lockVar <- newTMVarIO ()
    timer <- newTimer timeout $ timerAction stateVar lockVar
    atomically $ putTMVar stateVar WidgetState
        { wsContent = Nothing
        , wsDeath   = WDAlive
        }
    startTimer timer
    debugM "StatusBar.Widget.newWidget" $ "Exit"
    return $ Widget name wid timer stateVar lockVar
    where
        -- Action to run when the widget's timer fires. Move the widget further
        -- toward death and mark it updated.
        timerAction :: TMVar WidgetState -> LockVar -> IO Bool
        timerAction stateVar lockVar = do
            debugM "StatusBar.Widget.timerAction" $ "Enter"
            continue <- atomically $ do
                state <- takeTMVar stateVar
                let newDeath = advanceDeath $ wsDeath state
                -- Update the widget with the new state and timer information
                putTMVar stateVar state {wsDeath = newDeath}
                -- Try to mark the widget for update, but it may already be marked.
                tryPutTMVar lockVar ()
                -- If the widget is dead, stop the timer.
                return $ newDeath /= WDDead
            debugM "StatusBar.Widget.timerAction" $ "Return value: " ++ show continue
            debugM "StatusBar.Widget.timerAction" $ "Exit"
            return continue

        -- Move the widget ever on toward the end of its life
        advanceDeath :: WidgetDeath -> WidgetDeath
        advanceDeath WDAlive = WDDying
        advanceDeath WDDying = WDDead
        advanceDeath WDDead  = WDDead

updateWidget :: Widget       -- ^ The widget to touch
             -> Maybe String -- ^ The optional new content
             -> IO ()
updateWidget (Widget _ _ timer stateVar lockVar) mContent = do
    debugM "StatusBar.Widget.updateWidget" $ "Enter"
    stopTimer timer
    join . atomically $ do
        state <- takeTMVar stateVar
        let newState = chooseContent mContent $ revertDeath state
        putTMVar stateVar newState
        -- Try to mark the widget for update, but it may already be marked.
        tryPutTMVar lockVar ()
        -- If the widget is not dead, start the timer again.
        return $ when (wsDeath newState /= WDDead) $ startTimer timer
    debugM "StatusBar.Widget.updateWidget" $ "Exit"
    where
        -- Replace the widget's content only if there is new content available.
        chooseContent :: Maybe String -> WidgetState -> WidgetState
        chooseContent Nothing  state = state
        chooseContent mContent state = state {wsContent = mContent}

        -- Give the widget a new lease on life, as long as it isn't already
        -- dead.
        revertDeath :: WidgetState -> WidgetState
        revertDeath state
            | wsDeath state == WDDead = state
            | otherwise               = state {wsDeath = WDAlive}

widgetHasId :: Int -> Widget -> Bool
widgetHasId qWid (Widget _ wid _ _ _) = qWid == wid

-- | Show a widget by just showing its content
widgetStatus :: Widget -> STM (Maybe String)
widgetStatus (Widget name _ _ stateVar _) = getStatus name <$> readTMVar stateVar
    where
        getStatus :: String -> WidgetState -> Maybe String

        getStatus name (WidgetState {wsDeath = WDDead}) = Nothing

        getStatus name (WidgetState {wsContent = Nothing}) =
            Just $ dzenColorFG disabledColor . wrap "[" "]" $ name

        getStatus name (WidgetState {wsContent = (Just content), wsDeath = WDAlive}) =
            Just content

        getStatus name (WidgetState {wsContent = (Just content), wsDeath = WDDying}) =
            Just $ dzenColorFG disabledColor . dzenRemoveColor $ content

-- | Decide if a widget is dead, i.e., should be removed from the bar
widgetIsDead :: Widget -> STM Bool
widgetIsDead (Widget _ _ _ stateVar _) = do
    state <- readTMVar stateVar
    return $ wsDeath state == WDDead

