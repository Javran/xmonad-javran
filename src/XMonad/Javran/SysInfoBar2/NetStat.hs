module XMonad.Javran.SysInfoBar2.NetStat
  ( NetStat
  ) where

import Control.Concurrent

import XMonad.Javran.SysInfoBar.NetStat (getRxTxInfo)
import XMonad.Javran.SysInfoBar.DzenRender (renderNetStat)
import XMonad.Javran.SysInfoBar2.Types

data NetStat

instance Worker NetStat where
  workerStart _ sendMessage = getRxTxInfo >>= run
    where
      run (oldRx, oldTx) = do
        threadDelay 1000000
        p@(rx, tx) <- getRxTxInfo
        let rendered = renderNetStat (rx - oldRx, tx - oldTx)
        sendMessage (MPRendered (Just rendered))
        run p

  workerDeadline _ = 5

