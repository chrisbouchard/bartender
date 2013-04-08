-- | A server to listen for and handle requests to update the status bar
module StatusBar.Server where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.STM

import Network.Socket

import System.IO
import System.Log.Logger
import System.Log.Handler.Simple

import StatusBar.Request

-- | Start a UDP server that listens for requests from clients
serveBar :: Maybe String   -- ^ The hostname to bind
         -> Maybe String   -- ^ The port to bind
         -> Int            -- ^ The number of connections to queue
         -> RequestHandler -- ^ A request handler
         -> IO ()
serveBar mHostname mPort n requestHandler = withSocketsDo $ do
    debugM "ServerBar.Server.serveBar" $ "Enter"
    serveraddr <- head <$> getAddrInfo
        (Just defaultHints { addrFlags = [AI_PASSIVE] , addrSocketType = Datagram })
        mHostname mPort
    debugM "ServerBar.Server.serveBar" $ "serveraddr: " ++ show serveraddr

    sock <- socket (addrFamily serveraddr) (addrSocketType serveraddr) defaultProtocol
    debugM "ServerBar.Server.serveBar" $ "sock: " ++ show sock

    bindSocket sock (addrAddress serveraddr)
    debugM "ServerBar.Server.serveBar" $ "Bound socket"

    --listen sock n
    --debugM "ServerBar.Server.serveBar" $ "Listening on socket with n = " ++ show n

    procRequests serveraddr sock

    warningM "ServerBar.Server.serveBar" $ "This IO operation should not exit."
    debugM "ServerBar.Server.serveBar" $ "Exit"
    where
        bufferSize = 1024

        procRequests :: AddrInfo -> Socket -> IO ()
        procRequests serveraddr mastersock = do
            debugM "ServerBar.Server.procRequests" $ "Enter"

            forever $ do
                (content, _, clientaddr) <- recvFrom mastersock bufferSize
                let line = fst $ break (== '\n') content
                debugM "ServerBar.Server.procRequests" $ "Received on mastersock"
                debugM "ServerBar.Server.procRequests" $ "    content: " ++ content
                debugM "ServerBar.Server.procRequests" $ "    line: " ++ line
                debugM "ServerBar.Server.procRequests" $ "    clientaddr: " ++ show clientaddr

                forkIO $ procMessage mastersock clientaddr line

            warningM "ServerBar.Server.procRequests" $ "This IO operation should not exit."
            debugM "ServerBar.Server.procRequests" $ "Exit"

        procMessage :: Socket -> SockAddr -> String -> IO ()
        procMessage sock clientaddr content = do
            debugM "ServerBar.Server.procMessage" $ "Enter"
            let request = parseRequestFrom clientaddr content
            debugM "ServerBar.Server.procMessage" $ "Request: " ++ show request
            mResponse <- requestHandler request
            case mResponse of
                (Just response) -> do
                    let (Request _ message) = response
                    debugM "ServerBar.Server.procMessage" $ "Response: " ++ show response
                    void $ sendTo sock (show message ++ "\n") clientaddr
                Nothing         -> do
                    debugM "ServerBar.Server.procMessage" $ "No response: "
                    return ()
            debugM "ServerBar.Server.procMessage" $ "Exit"

        parseRequestFrom :: SockAddr -> String -> Request
        parseRequestFrom client = Request client . parseMessage

