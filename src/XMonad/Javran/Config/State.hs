{-# LANGUAGE DeriveDataTypeable #-}
module XMonad.Javran.Config.State
where

import XMonad.Core
import Data.Time.Clock
import Data.Time.Calendar

{- a data type used for blocking EWMH when xmonad is just started -}
newtype StartupTime
  = StartupTime UTCTime
  deriving (Typeable,Show)

-- not persistent state, and we should intialize
-- the value using the startup time
instance ExtensionClass StartupTime where
    initialValue = StartupTime $ UTCTime d dt
      where
        d = fromGregorian 1970 1 1
        dt = secondsToDiffTime 0
