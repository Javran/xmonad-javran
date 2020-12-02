{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE
    PartialTypeSignatures
  , OverloadedStrings
  , RecordWildCards
  #-}
module XMonad.Javran.Config
( myConfig
) where

-- TODO: xmonad restarter

import Data.Monoid
import System.IO

import XMonad
import XMonad.Layout.Fullscreen
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import Data.Time.Clock
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)

import XMonad.Javran.Config.Workspace
import XMonad.Javran.Config.State
import XMonad.Javran.Config.LogHook

import qualified XMonad.Javran.Config.Keys as ConfKeys
import qualified XMonad.Javran.Config.LayoutHook as LyH
import qualified XMonad.Javran.Config.ManageHook as MgmH

-- TODO: fullscreen without frame?
myConfig :: Handle -> XConfig _
myConfig dzenHandle = def
    { modMask = mod3Mask
    , terminal = "xfce4-terminal"
    , keys = ConfKeys.keys
    , manageHook = fullscreenManageHook <> manageDocks <> MgmH.manageHook
    , handleEventHook = fullscreenEventHook <> docksEventHook -- <> myEwmhDesktopsEventHook
    , layoutHook = LyH.layoutHook
    , logHook = mkLogHook dzenHandle <> ewmhDesktopsLogHook
    , focusedBorderColor = "cyan"
    , workspaces = workspaceIds
    , startupHook = myStartupHook <> ewmhDesktopsStartup
    }

myStartupHook :: X ()
myStartupHook = do
    StartupTime <$> liftIO getCurrentTime >>= XS.put
    safeSpawn "/bin/bash" ["/home/javran/.xmonad/on-startup.sh"]

myEwmhDesktopsEventHook :: Event -> X All
myEwmhDesktopsEventHook e@ClientMessageEvent{..} = do
    a_aw <- getAtom "_NET_ACTIVE_WINDOW"
    curTime <- liftIO getCurrentTime
    StartupTime starupTime <- XS.get
    -- prevernt ewmh for the first 5 sec window after startup.
    if ev_message_type == a_aw && curTime `diffUTCTime` starupTime <= 5.0
      then pure (All True)
      else ewmhDesktopsEventHook e
myEwmhDesktopsEventHook e = ewmhDesktopsEventHook e
