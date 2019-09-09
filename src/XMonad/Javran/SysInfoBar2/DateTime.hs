{-# LANGUAGE OverloadedStrings #-}
module XMonad.Javran.SysInfoBar2.DateTime
  ( DateTime
  ) where

import Control.Concurrent
import Control.Monad
import Data.Colour.SRGB
import Data.String
import Data.Time
import System.Dzen

import XMonad.Javran.SysInfoBar2.Types

data DateTime

renderDateTime :: (String, String) -> DString
renderDateTime (dateStr, timeStr) = dStr <> " " <> tStr
  where
    dStr = fg (sRGB24read "#80FFFF") (fromString dateStr)
    tStr = fg (sRGB24read "#FFFF80") (fromString timeStr)

instance Worker DateTime where
  workerStart _ sendMessage = do
    let sendMsg dS tS =
          sendMessage $
            MPRendered (Just $ renderDateTime (dS,tS))
    forever $ do
      t <- getZonedTime
      let dateStr = formatTime defaultTimeLocale "%_Y-%m-%d" t
          timeStr = formatTime defaultTimeLocale "%T" t
      sendMsg dateStr timeStr
      threadDelay 500000

  workerDeadline _ = 4
