module XMonad.Javran.Config.Workspace
  ( workspaceIds
  , workspaceName
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import XMonad.Core hiding (workspaces)

workspaceNames :: [String]
workspaceNames = words "any1 any2 msg1 ext1 ext2"

-- sadly WorkspaceId is just a type synonym to String,
-- so let's just use Int as String in hope of reducing computational work
-- when testing equalities
workspaceIds :: [WorkspaceId]
workspaceIds = zipWith (\x _ -> show x) [1 :: Int ..] workspaceNames

workspaces :: M.Map WorkspaceId String
workspaces = M.fromList (zip workspaceIds workspaceNames)

workspaceName :: String -> String
workspaceName n = fromMaybe "???" $ M.lookup n workspaces
