module XMonad.Javran.SysInfoBar where

import XMonad.Javran.SysInfoBar.CpuUsage (CpuUsageWorker)
import XMonad.Javran.SysInfoBar.Types
import Data.Default
import Data.Function
import Data.Typeable
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.Map.Strict as M

main :: IO ()
main = do
    mSt <- newMVar def
    _ <- forkIO $ runWorker (Proxy :: Proxy CpuUsageWorker) mSt
    fix $ \run -> do
      threadDelay 500000
      mv <- tryReadMVar mSt
      case mv of
        Just m
          | k <- typeRep (Proxy :: Proxy CpuUsageWorker)
          , (Just (SomeWorkerState s)) <- M.lookup k m -> pure ()
        _ -> pure ()
      run
