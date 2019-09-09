{-# LANGUAGE OverloadedStrings #-}
module XMonad.Javran.SysInfoBar2.Mpd
  ( Mpd
  ) where

import Control.Monad
import Control.Concurrent
import System.Dzen

import qualified Network.MPD as Mpd

import XMonad.Javran.SysInfoBar2.Types

renderMpd :: Maybe Mpd.State -> DString
renderMpd mpdSt = "[" <> st <> "]"
  where
    st =
      case mpdSt of
        Nothing -> "?"
        Just Mpd.Playing -> ">"
        Just Mpd.Stopped -> "|"
        Just Mpd.Paused -> "|"

data Mpd

instance Worker Mpd where
  workerStart _ sendMessage = forever $ do
    r <- either (const Nothing) (Just . Mpd.stState) <$> Mpd.withMPD Mpd.status
    sendMessage (MPRendered (Just (renderMpd r)))
    threadDelay 500000

  workerDeadline _ = 5
