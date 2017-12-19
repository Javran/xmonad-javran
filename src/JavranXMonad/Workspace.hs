module JavranXMonad.Workspace
  ( myWorkspaceConf
  , workspaceName
  ) where

import Data.Maybe (fromMaybe)

wkSpaceAny1 :: String
wkSpaceAny1 = "any"

wkSpaceAny2 :: String
wkSpaceAny2 = "any"

wkSpaceIM   :: String
wkSpaceIM   = "msg"

wkSpaceExt1 :: String
wkSpaceExt1 = "ext"

wkSpaceExt2 :: String
wkSpaceExt2 = "ext"

-- fst: the actual workspace name shown in config
-- snd: a short description, should be exactly of length 3
myWorkspaceList :: [(String, String)]
myWorkspaceList = zip numList wkSpaceDescriptionList
    where
        numList = map show [1..]
        wkSpaceDescriptionList =
            [ wkSpaceAny1
            , wkSpaceAny2
            , wkSpaceIM
            , wkSpaceExt1
            , wkSpaceExt2
            ]

myWorkspaceConf :: [String]
myWorkspaceConf = map fst myWorkspaceList

workspaceName :: String -> String
workspaceName n = fromMaybe "???" $ lookup n myWorkspaceList
