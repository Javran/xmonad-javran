module XMonad.Javran.Config.ManageHook
  ( manageHook
  ) where

import Data.Monoid
import Data.List

import XMonad hiding (manageHook)
import XMonad.Hooks.ManageHelpers

-- command `xprop WM_CLASS` would give you a hint on `className` below
manageHook :: Query (Endo WindowSet)
manageHook = composeAll
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
