module JavranXMonad.Config
( myConfig
, initScript
, dzenCommand
, conkyCommand
) where

import Data.Ratio
import XMonad
import XMonad.Core
import XMonad.Layout.Fullscreen
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.CustomKeys
import System.IO
import System.FilePath

import qualified XMonad.StackSet as W

initScript xmBase = xmBase </> "xmonad-init.sh"

pathStreamConvert xmBase = xmBase </> "StreamConvert"
pathStreamConvertConf xmBase = xmBase </> "stream_convert.txt"

conkyConf xmBase = xmBase </> "conky-json.conf"

dzenCommand = unwords
    [ "dzen2"
    , "-w" , show 900
    , "-ta", "l"
    , "-h" , show 24
    , "-fg", "\"#22EE11\""
    , "-bg", "\"#202020\""
    , "-fn", "\"WenQuanYi MicroHei Mono:pixelsize=15:antialias=true\""
    ]

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
    , "-w", show 810
    , "-x", show 901
    , "-h", show 24
    , "-fn", "\"DejaVu Sans Mono:pixelsize=15:antialias=true\""
    , "-bg", "\"#505050\""
    ]

myManageHook = composeAll
    [ className =? "Gimp"     --> doFloat
    , className =? "Pidgin"   --> doShift "3:m"
    ]

-- TODO: let workspace name be numbers,
--   and we "translate" numbers to readable names as needed
--   e.g.:
--   instead of:
--   [1:a] 3:m : Tall : xxxxx
--   have:
--   [1] 3 : a : Tall : xxxxx

myWorkspace = zipWith combine [1..] wkSpaceNames
    where 
        combine n name = show n ++ ':' : name
        wkSpaceNames =
            [ "a" -- anything
            , "a" -- anything
            , "m" -- instant messages
            , "e" -- extended
            , "e" -- extended
            ]

defaultLayoutHook = layoutHook defaultConfig

myLayoutHook = fullscreenFull $ avoidStruts mainLayout
    where
        imLayout = withIM (1%7) (Role "buddy_list") defaultLayoutHook
        mainLayout = onWorkspace "3:m" imLayout $ defaultLayoutHook

myConfig dzenHandle = defaultConfig
    { modMask = mod3Mask
    , terminal = "xfce4-terminal"
    , keys = customKeys (const []) insKeys
    , manageHook = manageDocks <+> fullscreenManageHook <+> myManageHook
    , handleEventHook = fullscreenEventHook
    , layoutHook = myLayoutHook
    , logHook = myLogHook dzenHandle
    , focusedBorderColor = "cyan"
    , workspaces = myWorkspace
    }

myLogHook h = dynamicLogWithPP $ defaultPP { ppOutput = hPutStrLn h . strOp }
    where strOp str = str -- TODO left dzen bar

insKeys :: XConfig l -> [((KeyMask, KeySym), X ())]
insKeys conf@(XConfig {modMask = modm, workspaces = wkSpace}) =
    [ ((mod4Mask, xK_w          ) , spawn "firefox-bin") 
    , ((mod4Mask, xK_r          ) , spawn "xfce4-terminal")
    , ((mod4Mask, xK_e          ) , spawn "thunar")
    , ((mod4Mask, xK_l          ) , spawn "xscreensaver-command --lock")
    , ((mod4Mask, xK_m          ) , spawn "gmpc")
    , ((mod4Mask, xK_apostrophe ) , spawn "amixer set Master 5+")
    , ((mod4Mask, xK_semicolon  ) , spawn "amixer set Master 5-")
    , ((mod4Mask, xK_comma      ) , spawn "mpc prev")
    , ((mod4Mask, xK_period     ) , spawn "mpc next")
    , ((mod4Mask, xK_slash      ) , spawn "mpc toggle")
    ]
    ++ workspaceSwitchAltKeys modm wkSpace

workspaceSwitchAltKeys modMask wkSpace =
    [((modMask, k), windows $ W.shift i) | (i, k) <- zip wkSpace [xK_F1 .. xK_F12]]
