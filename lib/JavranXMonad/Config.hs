module JavranXMonad.Config
( myConfig
, initScript
, dzenCommand
, conkyCommand
) where

import Codec.Binary.UTF8.String (encodeString)
import Data.Function (on)
import Data.List (intercalate , sortBy)
import Data.Maybe (isJust)
import Data.Monoid (Endo)
import Data.Ratio ((%))
import System.Cmd (system)
import System.FilePath ((</>))
import System.IO (hPutStrLn, Handle)
import XMonad
import XMonad.Layout.Fullscreen
    ( fullscreenEventHook
    , fullscreenFull
    , fullscreenManageHook
    )
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.IM (withIM, Property(..))
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Hooks.DynamicLog (dzenEscape)
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
import XMonad.Util.CustomKeys (customKeys)
import XMonad.Util.NamedWindows (getName)

import qualified XMonad.StackSet as W

import JavranXMonad.Workspace

initScript            :: FilePath -> FilePath
conkyConf             :: FilePath -> FilePath
pathStreamConvert     :: FilePath -> FilePath
pathStreamConvertConf :: FilePath -> FilePath
initScript            xmBase = xmBase </> "xmonad-init.sh"
pathStreamConvert     xmBase = xmBase </> "StreamConvert"
pathStreamConvertConf xmBase = xmBase </> "stream_convert.txt"
conkyConf             xmBase = xmBase </> "conky-json.conf"

dzenCommand :: String
dzenCommand = unwords
    [ "dzen2"
    , "-w" , show 900
    , "-ta", "l"
    , "-h" , show 24
    , "-fg", "\"#22EE11\""
    , "-bg", "\"#202020\""
    , "-fn", "\"WenQuanYi MicroHei Mono:pixelsize=15:antialias=true\""
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
    , "-w", show 810
    , "-x", show 900
    , "-h", show 24
    , "-fn", "\"DejaVu Sans Mono:pixelsize=15:antialias=true\""
    , "-bg", "\"#505050\""
    ]

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
    [ className =? "Gimp"     --> doFloat
    -- TODO: "3"
    , className =? "Pidgin"   --> doShift "3"
    ]

-- TODO: can I switch to the corresponding workspace
--   when I click something on the trayer which requires focus?

defaultLayoutHook = layoutHook defaultConfig

myLayoutHook = fullscreenFull $ avoidStruts mainLayout
    where
        imLayout = withIM (1%7) (Role "buddy_list") (Grid ||| Mirror Grid)
        -- TODO: "3"
        mainLayout = onWorkspace "3" imLayout defaultLayoutHook

myConfig dzenHandle = defaultConfig
    { modMask = mod3Mask
    , terminal = "xfce4-terminal"
    , keys = customKeys (const []) insKeys
    , manageHook = manageDocks <+> fullscreenManageHook <+> myManageHook
    , handleEventHook = fullscreenEventHook
    , layoutHook = myLayoutHook
    , logHook = myLogHook dzenHandle
    , focusedBorderColor = "cyan"
    , workspaces = myWorkspaceConf
    }

dzenColorize :: String -> String -> String
dzenColorize colorStr str = concat
    [ "^fg("
    , colorStr
    , ")"
    , str
    , "^fg()"
    ]

myLogHook :: Handle -> X ()
myLogHook h = do
    -- retrieve states that we might use
    -- mutable   => state
    -- immutable => xConf
    state <- get
    xConf <- ask

    let curWindowSet = windowset state

    windowTitle <- maybe
        -- no focus
        (return "<Nothing>") 
        -- or try to figure out its title
        (fmap show . getName) . W.peek 
        $ curWindowSet

    let curWorkspaceTags = workspaces $ config xConf
    -- wwis : Workspaces that has some Window Inside 
    let wwis = map W.tag $ filter hasSomeWindows $ allWorkspacesInst curWindowSet
    let curWorkspaceTag = W.currentTag curWindowSet
    let outStr = encodeString $ intercalate " | "
                [ dzenColorize "#FFFFFF" $ intercalate "" $ map (workspaceRepresent curWorkspaceTag wwis) curWorkspaceTags
                , dzenColorize "#FF6600" $ dzenEscape $ workspaceName curWorkspaceTag
                , dzenColorize "#33FFFF" $ dzenEscape $ windowTitle
                ]

    io $ hPutStrLn h outStr
    where
        hasSomeWindows = isJust . W.stack
        workspaceRepresent wTag wwis w
            | w == wTag     =  w  -- current workspace: show its tag
            | w `elem` wwis = "*" -- has some window inside
            | otherwise     = "." -- normal

        -- get all workspace instances from stack set
        --   note that this might not be the order defined by config
        allWorkspacesInst s = map W.workspace (W.current s : W.visible s) ++ W.hidden s

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

workspaceSwitchAltKeys :: KeyMask  -> [WorkspaceId] -> [((KeyMask, KeySym), X ())]
workspaceSwitchAltKeys modMask wkSpace =
    [((modMask, k), windows $ W.shift i) | (i, k) <- zip wkSpace [xK_F1 .. xK_F12]]
