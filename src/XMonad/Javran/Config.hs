{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE PartialTypeSignatures, OverloadedStrings, RecordWildCards #-}
module XMonad.Javran.Config
( myConfig
, initScript
, dzenCommand
, conkyCommand
) where

-- TODO: xmonad restarter

import Data.Monoid
import Data.List
import System.IO

import XMonad
import XMonad.Layout.Fullscreen
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import Data.Time.Clock
import System.FilePath.Posix
import XMonad.Hooks.ManageHelpers
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)

import qualified XMonad.Javran.Config.Keys as ConfKeys
import XMonad.Javran.Config.Workspace
import XMonad.Javran.Config.State
import XMonad.Javran.Config.LogHook
import qualified XMonad.Javran.Config.LayoutHook as LyH

initScript             :: FilePath -> FilePath
conkyConf              :: FilePath -> FilePath
pathStreamConvert      :: FilePath -> FilePath
pathStreamConvertConf  :: FilePath -> FilePath
initScript             = (</> "xmonad-init.sh")
pathStreamConvert      = (</> "StreamConverter")
pathStreamConvertConf  = (</> "stream_convert.txt")
conkyConf              = (</> "conky-json.lua")

showI :: Int -> String
showI = show

dzenCommand :: String
dzenCommand = unwords
    [ "dzen2"
    , "-x" , showI 0
    , "-w" , showI 900
    , "-ta", "l"
    , "-h" , showI 24
    , "-fg", "\"#22EE11\""
    , "-bg", "\"#202020\""
    , "-fn", "\"WenQuanYi MicroHei Mono:pixelsize=15:antialias=true\""
    , "-e", "\"button2=;\""
    --, "-l", "5"
    ]

conkyCommand :: FilePath -> String
conkyCommand xmPath = unwords
    [ "pkill -9 conky"
    , ";"
    , "conky"
    , "-c", conkyConf xmPath
    , "|"
    , pathStreamConvert xmPath
    , pathStreamConvertConf xmPath
    , "|"
    , "dzen2"
    , "-w", showI 810
    , "-x", showI 900
    , "-h", showI 24
    , "-fn", "\"DejaVu Sans Mono:pixelsize=15:antialias=true\""
    , "-bg", "\"#505050\""
    , "-e", "\"button2=;\""
    -- , "-l", "4"
    ]

-- command `xprop WM_CLASS` would give you a hint on `className` below
myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
    [ className =? "Gimp"     --> doFloat
    -- TODO: "3"
    , isSplash --> doFloat
    , className =? "Pidgin"   --> doShift "3"
    , className =? "net-minecraft-MinecraftLauncher" --> doFloat
    , className =? "Gnuplot" --> doFloat
    , className =? "FLTK" --> doFloat
    , className =? "poi"   --> doFloat
    , className =? "poi"   --> doShift "4"
    -- not necessary
    -- , className =? "jetbrains-android-studio" --> doFloat
    , className =? "Xfce4-appfinder" --> doFloat
    , className =? "Nm-connection-editor" --> doFloat
    , fmap ("@dev" `isPrefixOf`) title --> doFloat
    , ((&&) <$> fmap (== "Thunar") className
            <*> fmap (== "File Operation Progress") title)
       --> doFloat
    , isJuliaImageView --> doFloat
    , title =? "PLT Redex Reduction Graph" --> doFloat
    , fmap ("Fcitx" `isPrefixOf`) title --> doFloat
    , fmap ("Fcitx" `isPrefixOf`) className --> doFloat
    , stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog" --> doFloat
    ]
  where
    isJuliaImageView :: Query Bool
    isJuliaImageView = (== "ImageView") <$> title

    isSplash :: Query Bool
    isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"

-- TODO: fullscreen without frame?
myConfig :: Handle -> XConfig _
myConfig dzenHandle = myEwmh $ def
    { modMask = mod3Mask
    , terminal = "xfce4-terminal"
    , keys = ConfKeys.keys
    , manageHook = fullscreenManageHook <+> manageDocks <+> myManageHook
    , handleEventHook = fullscreenEventHook <+> docksEventHook
    , layoutHook = LyH.layoutHook
    , logHook = mkLogHook dzenHandle
    , focusedBorderColor = "cyan"
    , workspaces = workspaceIds
    , startupHook = myStartupHook
    }

myEwmh :: XConfig a -> XConfig a
myEwmh XConfig {..} = XConfig
    { startupHook = startupHook <> ewmhDesktopsStartup
    , handleEventHook = handleEventHook <> myEwmhDesktopsEventHook
    , logHook = logHook <> ewmhDesktopsLogHook
    , ..
    }

myStartupHook :: X ()
myStartupHook = do
    StartupTime <$> liftIO getCurrentTime >>= XS.put
    safeSpawn "/bin/bash" ["/home/javran/.xmonad/on-startup.sh"]

myEwmhDesktopsEventHook :: Event -> X All
myEwmhDesktopsEventHook e@ClientMessageEvent
    {ev_message_type = mt} = do
    a_aw <- getAtom "_NET_ACTIVE_WINDOW"
    curTime <- liftIO getCurrentTime
    StartupTime starupTime <- XS.get
    -- prevernt ewmh for the first 5 sec window after startup.
    if mt == a_aw && curTime `diffUTCTime` starupTime <= 5.0
      then pure (All True)
      else ewmhDesktopsEventHook e
myEwmhDesktopsEventHook e = ewmhDesktopsEventHook e
