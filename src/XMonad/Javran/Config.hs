{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE PartialTypeSignatures, OverloadedStrings #-}
module XMonad.Javran.Config
( myConfig
, initScript
, dzenCommand
, conkyCommand
) where

-- TODO: xmonad restarter

import Data.Maybe (isJust, fromMaybe)
import Data.Ratio ((%))
import XMonad
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.IM (withIM, Property(..))
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageDocks
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run
import Data.Time.Clock
import System.FilePath.Posix
import Data.Colour.Names
import Data.Colour.SRGB

import XMonad.Hooks.ManageHelpers

import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import Data.Monoid
import qualified Data.Map.Strict as M

import Data.List

import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

import qualified System.Dzen as DZ

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
    let dzenOut = io . hPutStrLn h
    -- retrieve states that we might use
    -- mutable   => state
    -- immutable => xConf
    state' <- get
    xConf <- ask

    let curWindowSet = windowset state'

    windowTitle <- maybe
        -- no focus
        (pure "<Nothing>")
        -- or try to figure out its title
        (fmap (take 100 . show) . getName) . W.peek
        $ curWindowSet

    let layoutDesc =
          description . W.layout . W.workspace . W.current $ curWindowSet
        -- get all workspace instances from stack set
        --   note that this might not be the order defined by config
        allWorkspacesInst s = map W.workspace (W.current s : W.visible s) ++ W.hidden s
        hasSomeWindows = isJust . W.stack
        curWorkspaceTags = workspaces $ config xConf
        
        -- wwis : Workspaces that has some Window Inside
        wwis = map W.tag $ filter hasSomeWindows $ allWorkspacesInst curWindowSet
        curWorkspaceTag = W.currentTag curWindowSet
        sep :: DZ.DString
        sep = " | "
        {-
          representing a single workspace by one styled character
          current rules are:
          - a focus workspace gets a colored workspace tag
          - a workspace that contains at least one window get a normal color tag
          - otherwise "-"
         -}
        workspaceRep wTag w
          | w == wTag     = DZ.fg cyan $ DZ.str w
          | w `elem` wwis = DZ.str w
          | otherwise     = "-"

        workspaceInfo :: DZ.DString
        workspaceInfo = foldMap
          (workspaceRep curWorkspaceTag)
          curWorkspaceTags
        curWsName = DZ.str $ workspaceName curWorkspaceTag
        curLayout = DZ.str $ shortenLayoutDesc layoutDesc
        winTitle = DZ.str windowTitle
        dzOutData :: DZ.DString
        dzOutData = mconcat . intersperse sep $
            [ DZ.fg white workspaceInfo
            , DZ.fg (sRGB24 0xFF 0x66 0x00) curWsName
            , DZ.fg (sRGB24 0xFF 0x33 0x22) curLayout
            , DZ.fg (sRGB24 0x33 0xFF 0xFF) winTitle
            ]
    {-
      TODO: click to switch e.g. exec "xdotool key Hyper_L+5"
      to create clickable area we'll need to patch dzen-utils though
    -}
    {-
      <workspaceInfo> <curWsName> <curLayout> <winTitle>
      all seperated by <sep>
    -}
    dzenOut ("^tw()" ++ DZ.toString dzOutData)

myEwmhDesktopsEventHook :: Event -> X All
myEwmhDesktopsEventHook e@ClientMessageEvent
    {ev_message_type = mt} = do
    a_aw <- getAtom "_NET_ACTIVE_WINDOW"
    curTime <- liftIO getCurrentTime
    StartupTime starupTime <- XS.get
    if mt == a_aw && curTime `diffUTCTime` starupTime <= 5.0
      then pure (All True)
      else ewmhDesktopsEventHook e
myEwmhDesktopsEventHook e = ewmhDesktopsEventHook e
