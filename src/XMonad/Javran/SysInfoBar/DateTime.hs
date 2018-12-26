{-# LANGUAGE TypeFamilies #-}
module XMonad.Javran.SysInfoBar.DateTime
  ( DateTime
  ) where

import XMonad.Javran.SysInfoBar.Types
import Data.Typeable
import Control.Concurrent
import qualified Data.Map.Strict as M
import Data.Time
import Control.Monad

type DateTimeResult = (String, String)

runWorkerWith :: MVar BarState -> IO ()
runWorkerWith mv = forever $ do
  t <- getZonedTime
  let dateStr = formatTime defaultTimeLocale "%_Y-%m-%d" t
      timeStr = formatTime defaultTimeLocale "%T" t
      k = typeRep (Proxy :: Proxy DateTime)
      res = SomeWorkerState (St (dateStr, timeStr))
  modifyMVar_ mv (pure . M.insert k res)
  threadDelay 500000

data DateTime

instance Worker DateTime where
  data WState DateTime = St DateTimeResult
  type WStateRep DateTime = DateTimeResult
  runWorker _ = runWorkerWith
  getStateRep (St x) = x
