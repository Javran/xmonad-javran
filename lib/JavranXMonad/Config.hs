module JavranXMonad.Config
( myConfig
, initScript
) where

import XMonad
import XMonad.Util.CustomKeys

initScript = "${HOME}/.xmonad/xmonad-init.sh"

myConfig = defaultConfig
    { modMask = mod3Mask
    , terminal = "xterm"
    , keys = customKeys (const []) insKeys
    }

insKeys :: XConfig l -> [((KeyMask, KeySym), X ())]
insKeys conf@(XConfig {modMask = modm}) =
    [ ((mod4Mask, xK_w          ) , spawn "firefox-bin") 
    , ((mod4Mask, xK_r          ) , spawn "xterm")
    , ((mod4Mask, xK_e          ) , spawn "thunar")
    , ((mod4Mask, xK_l          ) , spawn "xscreensaver-command --lock")
    , ((mod4Mask, xK_m          ) , spawn "gmpc")
    , ((mod4Mask, xK_apostrophe ) , spawn "amixer set Master 5+")
    , ((mod4Mask, xK_semicolon  ) , spawn "amixer set Master 5-")
    , ((mod4Mask, xK_comma      ) , spawn "mpc prev")
    , ((mod4Mask, xK_period     ) , spawn "mpc next")
    , ((mod4Mask, xK_slash      ) , spawn "mpc toggle")
    ]
