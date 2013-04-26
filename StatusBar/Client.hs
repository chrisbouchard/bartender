-- | A client for the status bar server
module StatusBar.Client
    ( BarClient
    , connectClient
    , sendAlive
    , sendUpdate
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

data BarClientInfo = BarClientInfo String (Maybe BarServerInfo)

type BarClient a = StateT BarClientInfo IO a

-- | Connect a client to a status bar server
connectClient :: String -- ^ The hostname to bind
              -> String -- ^ The port to bind
              -> String -- ^ The client's name
              -> BarClient ()
connectClient hostname port name = do
    clientInfo <- liftIO . withSocketsDo $ do
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
        BarClientInfo name <$> handleAck sock message

    put clientInfo

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
sendAlive :: BarClient ()
sendAlive = do
    (BarClientInfo name mServerInfo) <- get
    liftIO $ case mServerInfo of
        Nothing -> do
            errorM "ServerBar.Client.sendUpdate" $ "Connection is closed"
        (Just serverInfo) -> do
            -- TODO: We should probably check this return value
            void $ send (serverSock serverInfo) . show $ RAlive (serverCid serverInfo)

-- | Send an update to the server
sendUpdate :: String -- ^ The content of the update
           -> BarClient ()
sendUpdate content = do
    (BarClientInfo name mServerInfo) <- get
    liftIO $ case mServerInfo of
        Nothing -> do
            errorM "ServerBar.Client.sendUpdate" $ "Connection is closed"
        (Just serverInfo) -> do
            -- TODO: We should probably check this return value
            void $ send (serverSock serverInfo) . show $ RUpdate (serverCid serverInfo) content

