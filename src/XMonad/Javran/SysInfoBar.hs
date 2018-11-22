module XMonad.Javran.SysInfoBar where

import XMonad.Javran.SysInfoBar.CpuUsage (CpuUsageWorker)
import XMonad.Javran.SysInfoBar.Types
import Data.Proxy
import Data.Default
import Control.Concurrent.MVar

main :: IO ()
main = do
  mSt <- newMVar def
  runWorker (Proxy :: Proxy CpuUsageWorker) mSt
