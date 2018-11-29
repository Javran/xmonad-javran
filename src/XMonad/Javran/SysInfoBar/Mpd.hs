{-# LANGUAGE TypeFamilies #-}
module XMonad.Javran.SysInfoBar.Mpd
  ( MpdWorker
  ) where

import XMonad.Javran.SysInfoBar.Types
import Data.Typeable
import Control.Concurrent
import Data.Function
import qualified Data.Map.Strict as M
import qualified Network.MPD as Mpd

runWorkerWith :: MVar BarState -> IO ()
runWorkerWith mv = fix $ \run -> do
  r <- either (const Nothing) (Just . Mpd.stState) <$> Mpd.withMPD Mpd.status
  let k = typeRep (Proxy :: Proxy MpdWorker)
      res = SomeWorkerState (St r)
  modifyMVar_ mv (pure . M.insert k res)      
  threadDelay 500000
  run

data MpdWorker

instance Worker MpdWorker where
  data WState MpdWorker = St (Maybe Mpd.State)
  type WStateRep MpdWorker = Maybe Mpd.State
  runWorker _ = runWorkerWith
  getStateRep (St s) = s
