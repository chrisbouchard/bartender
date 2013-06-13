-- | A server to listen for and handle requests to update the status bar
module BarTender.Server
    ( ServerOptions(..)
    , defaultServerOptions
    , serveBar
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.STM

import Network.Socket

import System.IO
import System.Log.Logger
import System.Log.Handler.Simple

import BarTender.Message

-- | Options for a bartender server
data ServerOptions = ServerOptions
    { serverHost       :: String -- ^ The hostname to bind the server to
    , serverPort       :: String -- ^ The port to bind the server on
    , serverBufferSize :: Int    -- ^ The size of the buffer for incoming packets
    }
    deriving (Eq, Show)

-- | The default options for a server. Users should override fields as needed.
defaultServerOptions :: ServerOptions
defaultServerOptions = ServerOptions
    { serverHost       = "localhost"
    , serverPort       = "9999"
    , serverBufferSize = 1024
    }

-- | Start a UDP server that listens for requests from clients
serveBar :: ServerOptions  -- ^ Options for the server
         -> MessageHandler -- ^ A request handler
         -> IO ()
serveBar options messageHandler = withSocketsDo $ do
    debugM "ServerBar.Server.serveBar" $ "Enter"
    debugM "ServerBar.Server.serveBar" $ "ServerOptions: " ++ show options

    serverinfo <- head <$> getAddrInfo
        (Just defaultHints { addrFlags = [AI_PASSIVE] , addrSocketType = Datagram })
        (Just $ serverHost options) (Just $ serverPort options)
    debugM "ServerBar.Server.serveBar" $ "serverinfo: " ++ show serverinfo

    sock <- socket (addrFamily serverinfo) (addrSocketType serverinfo) defaultProtocol
    debugM "ServerBar.Server.serveBar" $ "sock: " ++ show sock

    bind sock (addrAddress serverinfo)
    debugM "ServerBar.Server.serveBar" $ "Bound socket"

    socketLoop sock

    warningM "ServerBar.Server.serveBar" $ "This IO operation should not exit."
    debugM "ServerBar.Server.serveBar" $ "Exit"
    where
        socketLoop :: Socket -> IO ()
        socketLoop mastersock = do
            debugM "ServerBar.Server.SocketLoop" $ "Enter"

            forever $ do
                (content, _, clientaddr) <- recvFrom mastersock $ serverBufferSize options
                let line = fst $ break (== '\n') content
                debugM "ServerBar.Server.socketLoop" $ "Received on mastersock"
                debugM "ServerBar.Server.socketLoop" $ "    content: " ++ content
                debugM "ServerBar.Server.socketLoop" $ "    line: " ++ line
                debugM "ServerBar.Server.socketLoop" $ "    clientaddr: " ++ show clientaddr

                forkIO $ procMessage mastersock clientaddr line

            warningM "ServerBar.Server.socketLoop" $ "This IO operation should not exit."
            debugM "ServerBar.Server.socketLoop" $ "Exit"

        procMessage :: Socket -> SockAddr -> String -> IO ()
        procMessage sock clientaddr content = do
            debugM "ServerBar.Server.procMessage" $ "Enter"
            let request = parseMessage content
            debugM "ServerBar.Server.procMessage" $ "Request: " ++ show request
            mResponse <- messageHandler request
            debugM "ServerBar.Server.procMessage" $ "Response: " ++ show mResponse
            case mResponse of
                (Just response) -> void $ sendTo sock (show response ++ "\n") clientaddr
                Nothing         -> return ()
            debugM "ServerBar.Server.procMessage" $ "Exit"

