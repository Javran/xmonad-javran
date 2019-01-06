module XMonad.Javran.Config.Workspace
  ( workspaceIds
  , workspaceName
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import XMonad.Core hiding (workspaces)

workspaceNames :: [String]
workspaceNames = words "any1 any2 msg1 ext1 ext2"

workspaceIds :: [WorkspaceId]
workspaceIds = show <$> [1 :: Int ..]

workspaces :: M.Map WorkspaceId String
workspaces = M.fromList (zip workspaceIds workspaceNames)

workspaceName :: String -> String
workspaceName n = fromMaybe "???" $ M.lookup n workspaces
