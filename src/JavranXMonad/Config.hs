{-# LANGUAGE FlexibleContexts, BangPatterns #-}

module JavranXMonad.Config
( myConfig
, initScript
, dzenCommand
, conkyCommand
) where

import System.Exit
import Codec.Binary.UTF8.String (encodeString)
import Control.Applicative
import Control.Monad
import Data.Maybe (isJust, fromMaybe)
import Data.Ratio ((%))
import System.FilePath ((</>))
import XMonad
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.IM (withIM, Property(..))
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.NoBorders
import XMonad.Hooks.DynamicLog (dzenEscape)
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
import XMonad.Util.CustomKeys (customKeys)
import XMonad.Util.NamedWindows (getName)
import Data.Time.Clock
import System.FilePath.Posix
import Control.Concurrent

import XMonad.Hooks.ManageHelpers

import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import Data.Monoid

import Data.List

import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

import System.IO

import JavranXMonad.Workspace
import JavranXMonad.Utils
import JavranXMonad.State

initScript             :: FilePath -> FilePath
conkyConf              :: FilePath -> FilePath
pathStreamConvert      :: FilePath -> FilePath
pathStreamConvertConf  :: FilePath -> FilePath
initScript             xmBase = xmBase </> "xmonad-init.sh"
pathStreamConvert      xmBase = xmBase </> "StreamConverter"
pathStreamConvertConf  xmBase = xmBase </> "stream_convert.txt"
conkyConf              xmBase = xmBase </> "conky-json.conf"

showI :: Int -> String
showI = show
-- gimp layout?

leftScr :: Int
leftScr = 0

dzenCommand :: String
dzenCommand = unwords
    [ "dzen2"
    , "-x" , showI leftScr
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
    , "-x", showI (leftScr+ 900)
    , "-h", showI 24
    , "-fn", "\"DejaVu Sans Mono:pixelsize=15:antialias=true\""
    , "-bg", "\"#505050\""
    , "-e", "\"button2=;\""
    -- , "-l", "4"
    ]

instance Applicative Query where
    pure = return
    (<*>) = ap

isSplash :: Query Bool
isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"

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
    -- not necessary
    -- , className =? "jetbrains-android-studio" --> doFloat
    , className =? "Xfce4-appfinder" --> doFloat
    , className =? "Nm-connection-editor" --> doFloat
    , fmap ("@dev" `isPrefixOf`) title --> doFloat
    , ((&&) <$> fmap (== "Thunar") className
            <*> fmap (== "File Operation Progress") title)
       --> doFloat
    , title =? "PLT Redex Reduction Graph" --> doFloat
    , fmap ("Fcitx" `isPrefixOf`) title --> doFloat
    , fmap ("Fcitx" `isPrefixOf`) className --> doFloat
    ]

-- TODO: close windows in a more decent way.
myLayoutHook = fullscreenFull $ avoidStruts mainLayout
    -- smartBorders $ fullscreenFull $ avoidStruts mainLayout
    where
        imLayout = withIM (1%7) (Role "buddy_list") (Grid ||| Mirror Grid)
        -- TODO: "3"
        mainLayout = onWorkspace "3" imLayout defaultLayoutHook
        defaultLayoutHook = layoutHook defaultConfig

-- TODO: fullscreen without frame?
myConfig dzenHandle = myEwmh $ defaultConfig
    { modMask = mod3Mask
    , terminal = "xfce4-terminal"
    , keys = customKeys delKeys insKeys
    , manageHook = fullscreenManageHook <+> manageDocks <+> myManageHook
    , handleEventHook = fullscreenEventHook
    , layoutHook = myLayoutHook
    , logHook = myLogHook dzenHandle
    , focusedBorderColor = "cyan"
    , workspaces = myWorkspaceConf
    , startupHook = myStartupHook
    }

myEwmh :: XConfig a -> XConfig a
myEwmh c = c { startupHook     = startupHook c     <> ewmhDesktopsStartup
             , handleEventHook = handleEventHook c <> myEwmhDesktopsEventHook
             , logHook         = logHook c         <> ewmhDesktopsLogHook
             }

myStartupHook :: X ()
myStartupHook = StartupTime <$> liftIO getCurrentTime >>= XS.put

-- | colorize text in dzen
dzenColorize :: String -> String -> String
dzenColorize colorStr str = concat
    [ "^fg("
    , colorStr
    , ")"
    , str
    , "^fg()"
    ]

-- | make layout description shorter
shortenLayoutDesc :: String -> String
shortenLayoutDesc ld = keepStringLength 3 tooShortHdl tooLongHdl $ shortened ld
    where
        tooLongHdl = id -- take 3
        tooShortHdl s = padRight (3 - length s) ' ' s
        shortened s = fromMaybe s $ lookup s shortenDict

        shortenDict :: [( String, String )]
        shortenDict =
            [ ( "Full"
              , " F " )
            , ( "Tall"
              , " T " )
            , ( "Mirror Tall"
              , "M T")
            , ( "IM Grid"
              , "IMG" )
            , ("IM Mirror Grid"
              , "IMR" )
            ]

myLogHook :: Handle -> X ()
myLogHook h = do
    -- retrieve states that we might use
    -- mutable   => state
    -- immutable => xConf
    state' <- get
    xConf <- ask

    let curWindowSet = windowset state'

    windowTitle <- maybe
        -- no focus
        (return "<Nothing>")
        -- or try to figure out its title
        (fmap show . getName) . W.peek
        $ curWindowSet

    let layoutDescription = description . W.layout . W.workspace . W.current $ curWindowSet

    let curWorkspaceTags = workspaces $ config xConf
    -- wwis : Workspaces that has some Window Inside
    let wwis = map W.tag $ filter hasSomeWindows $ allWorkspacesInst curWindowSet
    let curWorkspaceTag = W.currentTag curWindowSet
    let outStr = encodeString $ intercalate " | "
                [ dzenColorize "#FFFFFF" $ intercalate "" $ map (workspaceRepresent curWorkspaceTag wwis) curWorkspaceTags
                , dzenColorize "#FF6600" $ dzenEscape $ workspaceName curWorkspaceTag
                , dzenColorize "#FF3322" $ dzenEscape $ shortenLayoutDesc layoutDescription
                , dzenColorize "#33FFFF" $ dzenEscape   windowTitle
                ]

    io $ hPutStrLn h ("^tw()" ++ outStr)
    where
        hasSomeWindows = isJust . W.stack
        workspaceRepresent wTag wwis w
            | w == wTag     =  w  -- current workspace: show its tag
            | w `elem` wwis = "*" -- has some window inside
            | otherwise     = "." -- normal

        -- get all workspace instances from stack set
        --   note that this might not be the order defined by config
        allWorkspacesInst s = map W.workspace (W.current s : W.visible s) ++ W.hidden s

-- | some key bindings
insKeys :: XConfig l -> [((KeyMask, KeySym), X ())]
insKeys (XConfig {modMask = modm, workspaces = wkSpace}) =
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
      -- TODO: no xmonad binary ...
    , ((modm    , xK_q          ) , spawn "xmonad --restart")
    , ((modm .|. shiftMask, xK_q     ), do
           xmHome <- getXMonadDir
           spawn $ "/bin/bash " ++ (xmHome </> "on-finalize.sh")
           -- TODO: doesn't seem to work
           -- right now it delays for 5 seconds and then terminates
           io $ do
               threadDelay $ 5 * 1000 * 1000
               exitSuccess
           return ())
    ]
    ++ workspaceSwitchAltKeys modm wkSpace

delKeys :: XConfig l -> [(KeyMask, KeySym)]
delKeys (XConfig {modMask = modm, workspaces = wkSpace}) =
    [ (modm              , xK_q     )
    , (modm .|. shiftMask, xK_q     )
    ]

-- | key bindings for moving windows around
workspaceSwitchAltKeys :: KeyMask  -> [WorkspaceId] -> [((KeyMask, KeySym), X ())]
workspaceSwitchAltKeys modMask' wkSpace =
    [((modMask', k), windows $ W.shift i) | (i, k) <- zip wkSpace [xK_F1 .. xK_F12]] ++
    [((modMask', k), windows $ W.shift i) | (i, k) <- zip wkSpace [xK_a, xK_s, xK_d, xK_f, xK_g]]

myEwmhDesktopsEventHook :: Event -> X All
myEwmhDesktopsEventHook e@(ClientMessageEvent
    {ev_message_type = mt}) = do
    a_aw <- getAtom "_NET_ACTIVE_WINDOW"
    curTime <- liftIO getCurrentTime
    StartupTime starupTime <- XS.get
    if    mt == a_aw
       && curTime `diffUTCTime` starupTime <= 5.0
       then return (All True)
       else ewmhDesktopsEventHook e
myEwmhDesktopsEventHook e = ewmhDesktopsEventHook e
