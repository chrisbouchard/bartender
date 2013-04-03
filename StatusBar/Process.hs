module StatusBar.Process where

import Control.Concurrent
import Control.Concurrent.STM.TMVar
import Control.Monad.STM

import System.IO.Unsafe

-- List of child semaphores to check when waiting
childListVar :: TMVar [TMVar ()]
childListVar = unsafePerformIO $ newTMVarIO []

-- Create a child thread and add a semaphore to the list
forkChild :: IO () -> IO (ThreadId)
forkChild action = do
    childVar <- newEmptyTMVarIO
    atomically $ do
        childList <- takeTMVar childListVar
        putTMVar childListVar $ childVar:childList
    forkFinally action $ \_ -> atomically $ putTMVar childVar ()

-- Wait on all the registered semaphores
waitForChildren :: STM ()
waitForChildren = do
    childList <- takeTMVar childListVar
    case childList of
        []               -> return ()
        childVar:remList -> do
            putTMVar childListVar remList
            takeTMVar childVar
            waitForChildren

