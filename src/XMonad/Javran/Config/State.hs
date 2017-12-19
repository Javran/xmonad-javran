{-# LANGUAGE DeriveDataTypeable #-}

module XMonad.Javran.Config.State
where

import XMonad.Core
import qualified XMonad.Util.ExtensibleState as XS
import Data.Time.Clock
import Data.Typeable
import Data.Time.Calendar

newtype StartupTime =
    StartupTime UTCTime
    deriving (Typeable,Show)

-- not persistent state, and we should intialize
-- the value using the startup time
instance ExtensionClass StartupTime where
    initialValue = StartupTime $ UTCTime d dt
      where
        d = fromGregorian 1970 1 1
        dt = secondsToDiffTime 0
