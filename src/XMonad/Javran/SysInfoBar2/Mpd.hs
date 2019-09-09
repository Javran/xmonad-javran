module XMonad.Javran.SysInfoBar2.Mpd
  ( Mpd
  ) where

import Control.Monad
import Control.Concurrent
import qualified Network.MPD as Mpd

import XMonad.Javran.SysInfoBar.Mpd ()
import XMonad.Javran.SysInfoBar.DzenRender (renderMpd)
import XMonad.Javran.SysInfoBar2.Types

data Mpd

instance Worker Mpd where
  workerStart _ sendMessage = forever $ do
    r <- either (const Nothing) (Just . Mpd.stState) <$> Mpd.withMPD Mpd.status
    sendMessage (MPRendered (Just (renderMpd r)))
    threadDelay 500000

  workerDeadline _ = 5
