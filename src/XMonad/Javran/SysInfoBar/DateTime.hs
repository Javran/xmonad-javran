{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
module XMonad.Javran.SysInfoBar.DateTime
  ( DateTime
  ) where

import XMonad.Javran.SysInfoBar.Types
import Data.Typeable
import Control.Concurrent
import qualified Data.Map.Strict as M
import Data.Time
import Control.Monad
import Data.Colour.SRGB
import qualified System.Dzen as Dz
import Data.String

type DateTimeResult = (String, String)

runWorkerWith :: MVar BarState -> IO ()
runWorkerWith mv = forever $ do
  t <- getZonedTime
  let dateStr = formatTime defaultTimeLocale "%_Y-%m-%d" t
      timeStr = formatTime defaultTimeLocale "%T" t
      k = typeRep (Proxy :: Proxy DateTime)
      res = SomeWorkerState (St (dateStr, timeStr))
  modifyMVar_ mv (pure . M.insert k res)
  threadDelay 500000

data DateTime

instance Worker DateTime where
  data WState DateTime = St DateTimeResult
  type WStateRep DateTime = DateTimeResult
  runWorker _ = runWorkerWith
  getStateRep (St x) = x

instance RenderableWorker DateTime where
  wRender _ (dateStr, timeStr) = dStr <> " " <> tStr
    where
      dStr = Dz.fg (sRGB24read "#80FFFF") (fromString dateStr)
      tStr = Dz.fg (sRGB24read "#FFFF80") (fromString timeStr)
