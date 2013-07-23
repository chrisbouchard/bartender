module BarTender.Process
    ( forkChild
    , forkChildFinally
    , killChildren
    , waitForChildren
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM.TMVar
import Control.Exception.Base
import Control.Monad
import Control.Monad.STM

import Data.Maybe

import System.IO.Unsafe

-- List of child semaphores to check when waiting
childListVar :: TMVar [(ThreadId, TMVar ())]
childListVar = unsafePerformIO $ newTMVarIO []

-- | Create a child thread that can be waited on by 'waitForChildren'. See
-- "Control.Concurrent.forkIO".
forkChild :: IO () -> IO ThreadId
forkChild action = forkChildFinally action $ \_ -> return ()

-- | Create a child thread that can be waited on by 'waitForChildren', and call
-- the supplied function when the thread is about to terminate. See
-- "Control.Concurrent.forkFinally".
forkChildFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkChildFinally action handler = do
    childVar <- newEmptyTMVarIO
    tid <- forkFinally action $ \e -> do
        handler e
        atomically $ putTMVar childVar ()
    atomically $ do
        childList <- takeTMVar childListVar
        putTMVar childListVar $ (tid, childVar) : childList
    return tid

-- | Kill all the child threads spawned by forkChild*.
killChildren :: IO ()
killChildren = do
    childList <- atomically $ swapTMVar childListVar []
    forM_ childList $ killThread . fst

-- | Wait on all the child threads spawned by forkChild*.
waitForChildren :: IO ()
waitForChildren = atomically $ waitForChildrenSTM
    where waitForChildrenSTM :: STM ()
          waitForChildrenSTM = do
              childList <- takeTMVar childListVar
              -- If there are no child processes, get out
              guard $ (childList /= [])
              -- Get the first child from the list
              let ((_, childVar) : remList) = childList
              readTMVar childVar
              putTMVar childListVar remList
              waitForChildrenSTM

