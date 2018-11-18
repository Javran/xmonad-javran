{-# LANGUAGE PartialTypeSignatures, OverloadedStrings #-}
module XMonad.Javran.Config.LogHook
  ( mkLogHook
  ) where

import Data.Maybe (isJust, fromMaybe)
import Data.List
import qualified Data.Map.Strict as M
import System.IO

import Data.Colour.Names
import Data.Colour.SRGB
import XMonad
import XMonad.Util.NamedWindows (getName)
import qualified XMonad.StackSet as W
import qualified System.Dzen as DZ

import XMonad.Javran.Config.Workspace
import XMonad.Javran.Utils

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
        , ( "IM Mirror Grid" , "IMR" )
        ]

    -- ld -> shortenDesc, if we happen to know how to shorten
    -- it properly
    shortenDesc = fromMaybe ld $ M.lookup ld knownShortens
    sLen = length shortenDesc

mkLogHook :: Handle -> X ()
mkLogHook h = do
    -- retrieve states that we might use
    -- mutable   => state
    -- immutable => xConf
    st <- get
    xConf <- ask

    let curWindowSet = windowset st

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
    io $ dzenPutLn h ("^tw()" ++ (DZ.toString dzOutData ++ "\n"))
