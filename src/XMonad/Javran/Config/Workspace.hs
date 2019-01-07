module XMonad.Javran.Config.Workspace
  ( workspaceIds
  , workspaceName
  ) where

import Data.Maybe (fromMaybe)
import XMonad.Core hiding (workspaces)

workspaceIds :: [WorkspaceId]
workspaceIds = fst <$> workspaceItems

workspaceItems :: [(WorkspaceId, String)]
workspaceItems = zip (show <$> [1 :: Int ..]) $ words "any1 any2 msg1 ext1 ext2"

workspaceName :: String -> String
workspaceName n = fromMaybe "???" $ lookup n workspaceItems
