module XMonad.Javran.SysInfoBar2.Battery
  ( Battery
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Function
import Data.Colour.SRGB
import System.Dzen
import XMonad.Javran.SysInfoBar.Battery (handleExc, getBatteryPath, BatState)
import XMonad.Javran.SysInfoBar.DzenRender (renderBattery)
import XMonad.Javran.SysInfoBar2.Types

data Battery

sleep :: IO ()
sleep = threadDelay 1000000

loopWithBatPath :: SendMessage -> FilePath -> Int -> Maybe DString -> IO ()
loopWithBatPath sendMessage batPath consecFailCount prevRendered = do
  -- if we are having consecutive failures,
  -- probably it's the problem of batPath we've bound to,
  -- in which case we should crash and allow starting from mainLoop again.
  when (consecFailCount > 32) $
    error "Too many failure."
  let getBatStat :: IO (Maybe BatState)
      getBatStat = do
        c <- read . head . lines <$> readFile (batPath ++ "/capacity")
        rawS <- readFile (batPath ++ "/status")
        pure (Just (c, rawS /= "Discharging\n" && rawS /= "Not charging\n"))
  r <- catch getBatStat (handleExc Nothing)
  case r of
    Nothing -> do
      sendMessage $ MPRendered prevRendered
      sleep
      loopWithBatPath sendMessage batPath (consecFailCount+1) prevRendered
    Just p -> do
      let rendered = renderBattery p
      sendMessage $ MPRendered $ Just rendered
      sleep
      loopWithBatPath sendMessage batPath 0 (Just $ fg (sRGB24read "#FF0000") rendered)

mainLoop :: SendMessage -> IO ()
mainLoop sendMessage = fix $ \loop -> do
  -- determine battery path on startup and then lock on it.
  mBatPath <- getBatteryPath
  case mBatPath of
    Nothing -> do
      -- send heartbeat and keep retrying.
      -- bring up a new thread is expensive than reusing one,
      -- so let's just keep this thread up all the time.
      sendMessage (MPRendered Nothing)
      sleep
      loop
    Just batPath ->
      loopWithBatPath sendMessage batPath 0 Nothing

instance Worker Battery where
  workerStart _ = mainLoop
  workerDeadline _ = 5
