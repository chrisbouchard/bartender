module StatusBar.Widget where

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

type LockVar = TMVar ()

-- | A widget on the bar
data Widget = Widget
    { wName    :: String
    , wId      :: Int
    , wTimeout :: Int
    , wState   :: TMVar WidgetState
    , wLock    :: LockVar
    }

instance Eq Widget where
    (Widget _ lWid _ _ _) == (Widget _ rWid _ _ _) = lWid == rWid

instance Ord Widget where
    compare (Widget lName lWid _ _ _) (Widget rName rWid _ _ _) =
        compare (lName, lWid) (rName, rWid)

instance Show Widget where
    show (Widget name wid timeout _ _) = "(Widget " ++ name ++ " " ++ show wid ++ ")"

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

-- | The state of a widget
data WidgetState = WidgetState
    { wsContent :: Maybe String
    , wsTimer   :: Maybe TimerId
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
    wid <- atomically $ do
        nextId <- takeTMVar nextIdVar
        putTMVar nextIdVar (nextId + 1)
        return nextId
    stateVar <- newEmptyTMVarIO
    lockVar <- newTMVarIO ()
    tid <- startTimer timeout $ timerHandler stateVar lockVar timeout
    atomically $ putTMVar stateVar WidgetState
        { wsContent = Nothing
        , wsTimer   = Just tid
        , wsDeath   = WDAlive
        }
    return $ Widget name wid timeout stateVar lockVar

updateWidget :: Widget -- ^ The widget to touch
             -> Maybe String -- ^ The optional new content
             -> IO ()
updateWidget (Widget _ _ timeout stateVar lockVar) mContent = do
    newTid <- startTimer timeout $ timerHandler stateVar lockVar timeout
    join . atomically $ do
        state <- takeTMVar stateVar
        -- If an old timer exists, stop it.
        let stopOldTimerIO = fromMaybe (return ()) $ stopTimer <$> wsTimer state
        -- If the new timer is pointless, stop it
        let stopNewTimerIO = when (wsDeath state == WDDead) $ stopTimer newTid
        -- Compute the new widget state
        newState <- chooseContent mContent <$> case wsDeath state of
            WDDead -> return state {wsTimer = Nothing}
            _      -> return state {wsTimer = Just newTid, wsDeath = WDAlive}
        putTMVar stateVar newState
        tryPutTMVar lockVar ()
        return $ stopOldTimerIO >> stopNewTimerIO
    where
        chooseContent :: Maybe String -> WidgetState -> WidgetState
        chooseContent Nothing  state = state
        chooseContent mContent state = state {wsContent = mContent}

-- Timer handler that advances the death state of the widget
timerHandler :: TMVar WidgetState -> LockVar -> Int -> TimerId -> IO ()
timerHandler stateVar lockVar timeout tid = do
    -- Start the timer no matter what. If we don't want it, we'll get
    -- rid of it later
    debugM "StatusBar.Widget.timerHandler" $ "Enter"
    debugM "StatusBar.Widget.timerHandler" $ "Timer " ++ show tid ++ " fired"
    state <- atomically $ readTMVar stateVar
    when (fromMaybe False $ (tid ==) <$> wsTimer state) $ do
        debugM "StatusBar.Widget.timerHandler" $ "ID matches: " ++ show tid
        newTid <- startTimer timeout $ timerHandler stateVar lockVar timeout
        join . atomically $ do
            state <- takeTMVar stateVar
            let newDeath = advanceDeath $ wsDeath state
            let mTid = maybeFromDeath newDeath newTid
            -- Update the widget with the new state and timer information
            putTMVar stateVar state
                { wsDeath = newDeath
                , wsTimer = mTid
                }
            tryPutTMVar lockVar ()
            -- If the widget is dead, stop the timer. Otherwise don't do
            -- anything.
            return $ case newDeath of
                WDDead -> stopTimer newTid
                _      -> return ()
    debugM "StatusBar.Widget.timerHandler" $ "Exit"
    where
        -- Return the proper Maybe constructor depending on the widget's death
        -- state
        maybeFromDeath :: WidgetDeath -> a -> Maybe a
        maybeFromDeath WDDead = (\_ -> Nothing)
        maybeFromDeath _      = Just

        -- Move the widget ever on toward the end of its life
        advanceDeath :: WidgetDeath -> WidgetDeath
        advanceDeath WDAlive = WDDying
        advanceDeath WDDying = WDDead
        advanceDeath WDDead  = WDDead

