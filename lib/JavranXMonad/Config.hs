module JavranXMonad.Config
( myConfig
, initScript
, dzenCommand
, conkyCommand
) where

import Data.Maybe
import Data.List (intercalate,sortBy)
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
import XMonad.Util.WorkspaceCompare
import Data.Function (on)

import Codec.Binary.UTF8.String (encodeString)

import qualified XMonad.StackSet as W

import XMonad.Util.NamedWindows (getName)

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
    -- TODO hard-coded "3"
    , className =? "Pidgin"   --> doShift "3"
    ]

-- TODO: let workspace name be numbers,
--   and we "translate" numbers to readable names as needed
--   e.g.:
--   instead of:
--   [1:a] 3:m : Tall : xxxxx
--   have:
--   [1] 3 : a : Tall : xxxxx

-- TODO: can I switch to the corresponding workspace
--   when I click something on the trayer which requires focus?

myWorkspace = map show [1..5]

workspaceName "1" = "any" -- anything
workspaceName "2" = "any"
workspaceName "3" = "msg" -- instant messages
workspaceName "4" = "ext" -- extended
workspaceName "5" = "ext"
workspaceName _   = "???"

defaultLayoutHook = layoutHook defaultConfig

myLayoutHook = fullscreenFull $ avoidStruts mainLayout
    where
        imLayout = withIM (1%7) (Role "buddy_list") defaultLayoutHook
        -- TODO: hard-coded "3"
        mainLayout = onWorkspace "3" imLayout defaultLayoutHook

myConfig dzenHandle = defaultConfig
    { modMask = mod3Mask
    , terminal = "xfce4-terminal"
    , keys = customKeys (const []) insKeys
    , manageHook = manageDocks <+> fullscreenManageHook <+> myManageHook
    , handleEventHook = fullscreenEventHook
    , layoutHook = myLayoutHook
    -- , logHook = myLogHook dzenHandle
    , logHook = myLogHookTest dzenHandle
    , focusedBorderColor = "cyan"
    , workspaces = myWorkspace
    }

myLogHook h = dynamicLogWithPP $ defaultPP { ppOutput = hPutStrLn h . strOp }
    where strOp str = str -- TODO left dzen bar

myLogHookTest h = do
    wset <- gets windowset

    wt <- maybe (return "<Nothing>") (fmap show . getName) . W.peek $ wset

    c <- ask
    let curWorkspaces = workspaces $ config c 
    let swi = map W.tag $ filter (isJust . W.stack) $ getWorkspaceInsts wset
   
    sb <- getSortByIndex
    let wsCurrent = W.currentTag wset
    let outStr = dzenEscape $ encodeString $ intercalate " | "
                    [ intercalate "" $ map (compactedWorkspace wsCurrent swi) curWorkspaces
                    , workspaceName wsCurrent
                    , wt
                    ]

    io $ hPutStrLn h outStr
    where
        compactedWorkspace wCurrent swi w
            | w == wCurrent = wCurrent
            | w `elem` swi = "*"
            | otherwise = "."

getWorkspaceInsts s = map W.workspace (W.current s : W.visible s) ++ W.hidden s

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
