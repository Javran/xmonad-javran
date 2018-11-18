{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE PartialTypeSignatures, OverloadedStrings #-}
module XMonad.Javran.Config.LayoutHook
  ( layoutHook
  ) where

import Data.Ratio ((%))

import XMonad hiding (layoutHook)
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.IM (withIM, Property(..))
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageDocks

-- TODO: close windows in a more decent way.
layoutHook :: _
layoutHook = smartBorders $ fullscreenFull $ avoidStruts mainLayout
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
