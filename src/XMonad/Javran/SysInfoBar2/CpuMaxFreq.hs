module XMonad.Javran.SysInfoBar2.CpuMaxFreq
  ( CpuMaxFreq
  ) where

import Control.Monad
import Control.Concurrent
import Data.Colour.SRGB
import System.Dzen

import XMonad.Javran.SysInfoBar.CpuMaxFreq (getCpuMaxFreqGHz)
import XMonad.Javran.SysInfoBar.DzenRender (renderCpuMaxFreq)
import XMonad.Javran.SysInfoBar2.Types

data CpuMaxFreq

instance Worker CpuMaxFreq where
  workerStart _ sendMessage = forever $ do
    v <- getCpuMaxFreqGHz
    let rendered = fg (sRGB24read "#FF80A0") $ renderCpuMaxFreq v
    sendMessage (MPRendered (Just rendered))
    threadDelay 1000000

  workerDeadline _ = 5
