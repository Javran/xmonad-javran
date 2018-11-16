{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
module XMonad.Javran.Config
( myConfig
, initScript
, dzenCommand
, conkyCommand
) where

-- TODO: xmonad restarter

import Codec.Binary.UTF8.String (encodeString)
import Data.Maybe (isJust, fromMaybe)
import Data.Ratio ((%))
import XMonad
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.IM (withIM, Property(..))
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.NoBorders
import XMonad.Hooks.DynamicLog (dzenEscape)
import XMonad.Hooks.ManageDocks
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run
import Data.Time.Clock
import System.FilePath.Posix

import XMonad.Hooks.ManageHelpers

import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import Data.Monoid
import qualified Data.Map.Strict as M

import Data.List

import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

import System.IO

import qualified XMonad.Javran.Config.Keys as ConfKeys
import XMonad.Javran.Config.Workspace
import XMonad.Javran.Config.State
import XMonad.Javran.Utils

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

-- TODO: close windows in a more decent way.
myLayoutHook :: _
myLayoutHook = smartBorders $ fullscreenFull $ avoidStruts mainLayout
    where
        imLayout = withIM (1%7) (Role "buddy_list") (Grid ||| Mirror Grid)
        -- TODO: "3"
        mainLayout = onWorkspace "3" imLayout defaultLayoutHook
        defaultLayoutHook = tiled ||| Mirror tiled ||| noBorders Full
          where
            -- default tiling algorithm partitions the screen into two panes
            tiled   = Tall nmaster delta ratio
            -- The default number of windows in the master pane
            nmaster = 1
            -- Default proportion of screen occupied by master pane
            ratio   = 1/2
            -- Percent of screen to increment by when resizing panes
            delta   = 3/100


-- TODO: fullscreen without frame?
myConfig :: Handle -> XConfig _
myConfig dzenHandle = myEwmh $ def
    { modMask = mod3Mask
    , terminal = "xfce4-terminal"
    , keys = ConfKeys.keys
    , manageHook = fullscreenManageHook <+> manageDocks <+> myManageHook
    , handleEventHook = fullscreenEventHook <+> docksEventHook
    , layoutHook = myLayoutHook
    , logHook = myLogHook dzenHandle
    , focusedBorderColor = "cyan"
    , workspaces = workspaceIds
    , startupHook = myStartupHook
    }

myEwmh :: XConfig a -> XConfig a
myEwmh c = c { startupHook     = startupHook c     <> ewmhDesktopsStartup
             , handleEventHook = handleEventHook c <> myEwmhDesktopsEventHook
             , logHook         = logHook c         <> ewmhDesktopsLogHook
             }

myStartupHook :: X ()
myStartupHook = do
    safeSpawn "/bin/bash" ["/home/javran/.xmonad/on-startup.sh"]
    StartupTime <$> liftIO getCurrentTime >>= XS.put

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
shortenLayoutDesc ld = case sLen `compare` 3 of
    LT -> padRightCut ' ' 3 shortenDesc
    EQ -> shortenDesc
    GT -> take 3 shortenDesc
  where
    knownShortens :: M.Map String String
    knownShortens = M.fromList
        [ ( "Full" , " F " )
        , ( "Tall" , " T " )
        , ( "Mirror Tall" , "M T")
        , ( "IM Grid" , "IMG" )
        , ("IM Mirror Grid" , "IMR" )
        ]

    -- ld -> shortenDesc, if we happen to know how to shorten
    -- it properly
    shortenDesc = fromMaybe ld $ M.lookup ld knownShortens
    sLen = length shortenDesc

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
        (fmap (take 100 . show) . getName) . W.peek
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

myEwmhDesktopsEventHook :: Event -> X All
myEwmhDesktopsEventHook e@ClientMessageEvent
    {ev_message_type = mt} = do
    a_aw <- getAtom "_NET_ACTIVE_WINDOW"
    curTime <- liftIO getCurrentTime
    StartupTime starupTime <- XS.get
    if    mt == a_aw
       && curTime `diffUTCTime` starupTime <= 5.0
       then return (All True)
       else ewmhDesktopsEventHook e
myEwmhDesktopsEventHook e = ewmhDesktopsEventHook e
