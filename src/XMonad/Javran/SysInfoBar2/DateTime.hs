module XMonad.Javran.SysInfoBar2.DateTime
  ( DateTime
  ) where

import Control.Concurrent
import Data.Time
import Control.Monad

import XMonad.Javran.SysInfoBar2.Types
import XMonad.Javran.SysInfoBar.DzenRender (renderDateTime)

data DateTime

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
