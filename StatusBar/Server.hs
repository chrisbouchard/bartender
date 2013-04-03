-- | A server to listen for and handle requests to update the status bar
module StatusBar.Server where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.STM

import Network.BSD
import Network.Socket

import System.IO

import StatusBar.Request

-- | Start a UDP server that listens for requests from clients
serveBar :: String             -- ^ The port on which to bind the server
         -> (Request -> IO ()) -- ^ A request handler
         -> IO ()
serveBar port requestHandler = withSocketsDo $ do
    serveraddr <- head <$> getAddrInfo
        (Just defaultHints {addrFlags = [AI_PASSIVE]})
        Nothing
        (Just port)

    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bindSocket sock (addrAddress serveraddr)

    procRequests sock
    where
        procRequests mastersock = forever $ do
            (connsock, clientaddr) <- accept mastersock
            forkIO $ procMessages connsock clientaddr

        procMessages connsock clientaddr = do
            connhdl <- socketToHandle connsock ReadMode
            request <- parseRequestFrom clientaddr <$> hGetContents connhdl
            hClose connhdl
            requestHandler request

