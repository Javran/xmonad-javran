module XMonad.Javran.Config.Keys
  ( keys
  ) where

import System.FilePath.Posix
import System.Exit

import XMonad hiding (keys)
import qualified XMonad.StackSet as W
import qualified Data.Map as LM
import XMonad.Util.CustomKeys (customKeys)

keys :: XConfig Layout -> LM.Map (KeyMask, KeySym) (X ())
keys = customKeys delKeys insKeys

-- | some key bindings
insKeys :: XConfig l -> [((KeyMask, KeySym), X ())]
insKeys XConfig {modMask = modm, workspaces = wkSpace} =
    [ ((mod4Mask, xK_w          ) , spawn "vivaldi")
    , ((mod4Mask, xK_r          ) , spawn "xfce4-terminal")
    , ((mod4Mask, xK_e          ) , spawn "thunar")
    , ((mod4Mask, xK_l          ) , spawn "xscreensaver-command --lock")
    , ((mod4Mask, xK_m          ) , spawn "gmpc")
    , ((mod4Mask, xK_apostrophe ) , spawn "pactl set-sink-volume 0 +5%")
      -- "amixer set Master 5+")
    , ((mod4Mask, xK_semicolon  ) , spawn "pactl set-sink-volume 0 -5%")
      -- "amixer set Master 5-")
    , ((mod4Mask, xK_comma      ) , spawn "mpc prev")
    , ((mod4Mask, xK_period     ) , spawn "mpc next")
    , ((mod4Mask, xK_slash      ) , spawn "mpc toggle")
    , ((modm    , xK_q          ) , spawn "xmonad --restart && \
                                          \notify-send \"XMonad built on `date`\"")
      -- TODO: make and run!
    , ((modm .|. shiftMask, xK_q     ), do
           xmHome <- getXMonadDir
           spawn $ "/bin/bash " ++ (xmHome </> "on-finalize.sh")
           -- TODO: doesn't seem to work
           -- right now it delays for 5 seconds and then terminates
           io exitSuccess)
    ]
    ++ workspaceSwitchAltKeys
  where
    -- | key bindings for moving windows around
    workspaceSwitchAltKeys :: [((KeyMask, KeySym), X ())]
    workspaceSwitchAltKeys =
        [((modm, k), windows $ W.shift i) | (i, k) <- zip wkSpace [xK_F1 .. xK_F12]] ++
        [((modm, k), windows $ W.shift i) | (i, k) <- zip wkSpace [xK_a, xK_s, xK_d, xK_f, xK_g]]


delKeys :: XConfig l -> [(KeyMask, KeySym)]
delKeys XConfig {modMask = modm} =
    [ (modm              , xK_q)
    , (modm .|. shiftMask, xK_q)
    ]
