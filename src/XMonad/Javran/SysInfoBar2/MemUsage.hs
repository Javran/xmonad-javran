{-# LANGUAGE RecordWildCards #-}
module XMonad.Javran.SysInfoBar2.MemUsage
  ( MemUsage
  ) where

import Control.Monad
import Control.Concurrent

import XMonad.Javran.SysInfoBar2.Types
import XMonad.Javran.SysInfoBar.MemUsage (getMemInfoRaw, MemInfoRaw(..))
import XMonad.Javran.SysInfoBar.DzenRender (renderMemUsage)

data MemUsage

instance Worker MemUsage where
  workerStart _ sendMessage = forever $ do
    MemInfoRaw {..} <- getMemInfoRaw
    let numer = mTotal - mAvailable
        denom = mTotal
    sendMessage (MPRendered (Just (renderMemUsage (numer, denom))))
    threadDelay 1000000

  workerDeadline _ = 5
