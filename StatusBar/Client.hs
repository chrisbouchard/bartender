-- | A client for the status bar server
module StatusBar.Client
    ( BarClient
    , connectClient
    , runClient
    , touchClient
    , updateClient
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.STM

import Network.Socket

import System.IO
import System.Log.Logger
import System.Log.Handler.Simple

import StatusBar.Message

data BarServerInfo = BarServerInfo
    { serverSock    :: Socket
    , serverCid     :: Int
    , serverTimeout :: Int
    , serverVersion :: Int
    }

data BarClientInfo = BarClientInfo
    { clientName   :: String
    , clientServer :: Maybe BarServerInfo
    }

type BarClient m a = StateT BarClientInfo m a

runClient :: MonadIO m
          => String        -- ^ The name of the client
          -> BarClient m a -- ^ The bar client monad to run
          -> m a
runClient name client = evalStateT client (BarClientInfo name Nothing)

-- | Connect a client to a status bar server
connectClient :: MonadIO m
              => String    -- ^ The hostname to bind
              -> String    -- ^ The port to bind
              -> BarClient m ()
connectClient hostname port = do
    (BarClientInfo name _) <- get

    serverInfo <- liftIO . withSocketsDo $ do
        debugM "ServerBar.Client.connectBar" $ "Enter"
        serverinfo <- head <$> getAddrInfo
            (Just defaultHints { addrFlags = [AI_PASSIVE] , addrSocketType = Datagram })
            (Just hostname) (Just port)
        debugM "ServerBar.Client.connectBar" $ "serverinfo: " ++ show serverinfo

        let serveraddr = addrAddress serverinfo

        sock <- socket (addrFamily serverinfo) (addrSocketType serverinfo) defaultProtocol
        debugM "ServerBar.Client.connectBar" $ "sock: " ++ show sock

        connect sock serveraddr
        debugM "ServerBar.Client.connectBar" $ "Connected socket"

        send sock . show $ RInit name
        message <- parseMessage <$> recv sock bufferSize
        debugM "ServerBar.Client.connectBar" $ "Received from server: " ++ (show message)
        handleAck sock message

    put $ BarClientInfo name serverInfo

    where
        bufferSize = 1024

        handleAck :: Socket -> Message -> IO (Maybe BarServerInfo)

        handleAck sock (RAck cid timeout version) = return . Just $
            BarServerInfo { serverSock = sock
                          , serverCid = cid
                          , serverTimeout = timeout
                          , serverVersion = version
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
    (BarClientInfo name mServerInfo) <- get
    liftIO $ case mServerInfo of
        Nothing -> do
            errorM "ServerBar.Client.sendUpdate" $ "Connection is closed"
        (Just serverInfo) -> do
            -- TODO: We should probably check this return value
            void $ send (serverSock serverInfo) . show $ RAlive (serverCid serverInfo)

-- | Send an update to the server
updateClient :: MonadIO m
             => String    -- ^ The content of the update
             -> BarClient m ()
updateClient content = do
    (BarClientInfo name mServerInfo) <- get
    liftIO $ case mServerInfo of
        Nothing -> do
            errorM "ServerBar.Client.sendUpdate" $ "Connection is closed"
        (Just serverInfo) -> do
            -- TODO: We should probably check this return value
            void $ send (serverSock serverInfo) . show $ RUpdate (serverCid serverInfo) content

