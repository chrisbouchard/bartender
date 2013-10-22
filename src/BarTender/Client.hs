-- | A client for the status bar server
module BarTender.Client
    ( BarClient
    , ConnectionOptions (..)
    , defaultConnectionOptions
    , connectClient
    , runClient
    , touchClient
    , updateClient

    -- The following are from Template Haskell
    , connectHost
    , connectPort
    , connectRetries
    , connectTimeout
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception.Base
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.STM

import Data.Lens.Common
import Data.Lens.Template

import Network.Socket

import System.IO
import System.Log.Logger
import System.Log.Handler.Simple
import System.Timeout

import BarTender.Message

data BarServerInfo = BarServerInfo
    { _serverSock    :: Socket
    , _serverCid     :: Int
    , _serverTimeout :: Int
    , _serverVersion :: Int
    }
    deriving Show

data BarClientInfo = BarClientInfo
    { _clientName       :: String
    , _clientServer     :: Maybe BarServerInfo
    , _clientConnection :: Maybe ConnectionOptions
    }
    deriving Show

type BarClient m a = StateT BarClientInfo m a

data ConnectionOptions = ConnectionOptions
    { _connectHost    :: String
    , _connectPort    :: String
    , _connectRetries :: Int
    , _connectTimeout :: Int
    }
    deriving Show

-- | Default options for connecting to a server
defaultConnectionOptions :: ConnectionOptions
defaultConnectionOptions = ConnectionOptions
    { _connectHost    = "localhost"
    , _connectPort    = "9999"
    , _connectRetries = 3
    , _connectTimeout = 60
    }

$( makeLenses [''BarServerInfo, ''BarClientInfo, ''ConnectionOptions] )

-- | Run a client monad
runClient :: MonadIO m
          => String        -- ^ The name of the client
          -> BarClient m a -- ^ The bar client monad to run
          -> m a
runClient name client = evalStateT client (BarClientInfo name Nothing Nothing)

-- | Connect a client to a status bar server
connectClient :: MonadIO m
              => ConnectionOptions
              -> BarClient m ()
connectClient options = do
    barClientInfo <- get

    mServerInfo <- liftIO . withSocketsDo $ do
        debugM "ServerBar.Client.connectBar" $ "Enter"
        serverinfo <- head <$> getAddrInfo
            (Just defaultHints { addrFlags = [AI_PASSIVE] , addrSocketType = Datagram })
            (Just $ options ^. connectHost) (Just $ options ^. connectPort)
        debugM "ServerBar.Client.connectBar" $ "serverinfo: " ++ show serverinfo

        let serveraddr = addrAddress serverinfo

        debugM "ServerBar.Client.connectBar" $ "Attempting socket"
        sock <- socket (addrFamily serverinfo) (addrSocketType serverinfo) defaultProtocol
        debugM "ServerBar.Client.connectBar" $ "sock: " ++ show sock

        debugM "ServerBar.Client.connectBar" $ "Attempting connection"
        connect sock serveraddr

        debugM "ServerBar.Client.connectBar" $ "Connected socket"

        mMessage <- attempt (options ^. connectTimeout) (options ^. connectRetries) $ do
            debugM "ServerBar.Client.connectBar" $ "Sending init message"
            send sock . show . RInit $ barClientInfo ^. clientName
            message <- parseMessage <$> recv sock bufferSize
            debugM "ServerBar.Client.connectBar" $ "Received from server: " ++ (show message)
            return message

        case mMessage of
            Nothing      -> return Nothing
            Just message -> handleAck sock message

    modify $ clientServer ^= mServerInfo

    where
        bufferSize = 1024

        seconds = 10 ^ 6

        -- Try an action a given number of times, failing on exception or after
        -- a specified timeout.
        attempt :: Int -> Int -> IO a -> IO (Maybe a)
        attempt waitTime 0 action = return Nothing
        attempt waitTime n action = do
            result <- catch (timeout (waitTime * seconds) action)
                            (\(e :: IOException) -> return Nothing)
            case result of
                Just x  -> return $ Just x
                Nothing -> attempt waitTime (n - 1) action

        handleAck :: Socket -> Message -> IO (Maybe BarServerInfo)
        handleAck sock (RAck cid timeout version) = return . Just $
            BarServerInfo { _serverSock    = sock
                          , _serverCid     = cid
                          , _serverTimeout = timeout
                          , _serverVersion = version
                          }
        handleAck sock message = do
            shutdown sock ShutdownBoth
            errorM "ServerBar.Client.handleAck" $ "Server did not respond with Ack"
            errorM "ServerBar.Client.handleAck" $ "    response: " ++ show message
            return Nothing

-- | Send an alive message to the server
touchClient :: MonadIO m
            => BarClient m ()
touchClient = do
    barClientInfo <- get
    liftIO . withSocketsDo $ case barClientInfo ^. clientServer of
        Nothing -> do
            errorM "ServerBar.Client.sendUpdate" $ "Connection is closed"
        (Just serverInfo) -> do
            -- TODO: We should probably check this return value
            void $ send (serverInfo ^. serverSock) . show $ RAlive (serverInfo ^. serverCid)

-- | Send an update to the server
updateClient :: MonadIO m
             => String    -- ^ The content of the update
             -> BarClient m ()
updateClient content = do
    barClientInfo <- get
    liftIO . withSocketsDo $ case barClientInfo ^. clientServer of
        Nothing -> do
            errorM "ServerBar.Client.sendUpdate" $ "Connection is closed"
        (Just serverInfo) -> do
            -- TODO: We should probably check this return value
            void $ send (serverInfo ^. serverSock) . show $ RUpdate (serverInfo ^. serverCid) content

