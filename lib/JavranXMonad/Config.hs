module JavranXMonad.Config
( myConfig
, initScript
) where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Util.CustomKeys

import qualified XMonad.StackSet as W

initScript = "${HOME}/.xmonad/xmonad-init.sh"

myManageHook = composeAll
    [
    ]

myConfig = defaultConfig
    { modMask = mod3Mask
    , terminal = "xfce4-terminal"
    , keys = customKeys (const []) insKeys
    , manageHook = manageDocks <+> myManageHook
    }

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
