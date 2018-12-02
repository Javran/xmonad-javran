{-# LANGUAGE TypeFamilies #-}
module XMonad.Javran.SysInfoBar.Mpd
  ( Mpd
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
  let k = typeRep (Proxy :: Proxy Mpd)
      res = SomeWorkerState (St r)
  modifyMVar_ mv (pure . M.insert k res)      
  threadDelay 500000
  run

data Mpd

instance Worker Mpd where
  data WState Mpd = St (Maybe Mpd.State)
  type WStateRep Mpd = Maybe Mpd.State
  runWorker _ = runWorkerWith
  getStateRep (St s) = s
