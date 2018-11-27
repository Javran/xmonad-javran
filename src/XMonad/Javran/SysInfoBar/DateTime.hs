{-# LANGUAGE TypeFamilies #-}
module XMonad.Javran.SysInfoBar.DateTime
  ( DateTimeWorker
  ) where

import XMonad.Javran.SysInfoBar.Types
import Data.Typeable
import Control.Concurrent
import Data.Function
import qualified Data.Map.Strict as M
import Data.Time.Clock.POSIX
import Data.Time.Format

type DateTimeResult = (String, String)

runWorkerWith :: MVar BarState -> IO ()
runWorkerWith mv = fix $ \run -> do
  t <- getCurrentTime
  let dateStr = formatTime defaultTimeLocale "%_Y-%m-%d" t
      timeStr = formatTime defaultTimeLocale "%T" t
      k = typeRep (Proxy :: Proxy DateTimeWorker)
      res = SomeWorkerState (DTSt (dateStr, timeStr))
  modifyMVar_ mv (pure . M.insert k res)
  threadDelay 500000
  run

data DateTimeWorker

instance Worker DateTimeWorker where
  data WState DateTimeWorker = DTSt DateTimeResult
  type WStateRep DateTimeWorker = DateTimeResult
  runWorker _ = runWorkerWith
  getStateRep (DTSt x) = x
