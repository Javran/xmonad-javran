module JavranXMonad.Config
( myConfig
, initScript
, dzenCommand
, conkyCommand
) where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Util.CustomKeys
import XMonad.Hooks.DynamicLog
import System.IO

import qualified XMonad.StackSet as W

initScript = "${HOME}/.xmonad/xmonad-init.sh"

dzenCommand = unwords
    [ "dzen2"
    , "-w" , show 1000
    , "-ta", "l"
    , "-h" , show 24
    , "-bg", "\"#202020\""
    , "-fn", "\"WenQuanYi MicroHei:pixelsize=16:antialias=true\""
    ]

conkyCommand = unwords
    [ "pkill -9 conky"
    , ";"
    , "conky"
    , "-c", "${HOME}/.xmonad/conky-xmonad.conf"
    , "|"
    , "dzen2"
    , "-w", show 709
    , "-x", show 1000
    , "-h", show 24
    , "-fn", "\"DejaVu Sans Mono\""
    , "-bg", "\"#505050\""
    ]

myManageHook = composeAll
    [ className =? "Gimp"     --> doFloat
    , className =? "Pidgin"   --> doFloat
    ]

defaultLayoutHook = layoutHook defaultConfig

myConfig dzenHandle = defaultConfig
    { modMask = mod3Mask
    , terminal = "xfce4-terminal"
    , keys = customKeys (const []) insKeys
    , manageHook = manageDocks <+> myManageHook
    , layoutHook = avoidStruts (defaultLayoutHook)
    , logHook = myLogHook dzenHandle
    }

myLogHook h = dynamicLogWithPP $ defaultPP { ppOutput = (hPutStrLn h).strOp }
    where strOp str = str -- "|" ++ str ++ "|"

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
