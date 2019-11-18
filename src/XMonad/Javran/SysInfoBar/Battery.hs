{-# LANGUAGE
    LambdaCase
  , OverloadedStrings
  , BlockArguments
  #-}
module XMonad.Javran.SysInfoBar.Battery
  ( Battery
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Colour.SRGB
import Data.Function
import Data.String
import System.Directory
import System.Dzen
import Text.Printf

import XMonad.Javran.SysInfoBar.Types

{-

  ref: https://www.kernel.org/doc/Documentation/ABI/testing/sysfs-class-power

  - it seems reasonable to assume that we'll have at most one battery,
    and it's not going to change when the instance is running
    (it's still possible to remove and reattach the battery but the path won't change)
  - /sys/class/power_supply/<XXX>/type must be "Battery", and "BAT" is a prefix
  - /sys/class/power_supply/<XXX>/capacity: 0 ~ 100 (int)
  - /sys/class/power_supply/<XXX>/status: we just want to know whether it's charging:
    "Discharging" or "Not charging" will be negative indicators
    (Note: for some reason my laptop shows "Unknown" when power is plugged in.)

 -}

type BatState = (Int, Bool) -- (<capacity>, <isCharging?>)

data Battery

renderBattery :: (Int, Bool) -> DString
renderBattery (capa, charge) = chgRdr <> capRdr
  where
    chgRdr = if charge then "+" else "="
    capRdr =
      if capa == 100
        then "Ful"
        else fromString (printf "%2d%%" capa)

sysPowerSupply :: FilePath
sysPowerSupply = "/sys/class/power_supply/"

handleExc :: a -> IOException -> IO a
handleExc x _ =  pure x

getBatteryPath :: IO (Maybe FilePath)
getBatteryPath = catch tryGet (handleExc Nothing)
  where
    testProperBatDir :: FilePath -> IO (Maybe FilePath)
    testProperBatDir sub = catch doTest (handleExc Nothing)
      where
        doTest = do
          let batMagic = "BAT"
          guard (take (length batMagic) sub == batMagic)
          let path = sysPowerSupply ++ sub
          "Battery\n" <- readFile (path ++ "/type")
          pure (Just path)
    tryGet :: IO (Maybe FilePath)
    tryGet = do
      ps <- listDirectory sysPowerSupply
      fix (\run -> \case
          [] -> pure Nothing
          p:remainingPs -> testProperBatDir p >>= \r ->
              case r of
                Just _ -> pure r
                Nothing -> run remainingPs
          ) ps

sleep :: IO ()
sleep = threadDelay 1000000

loopWithBatPath :: SendMessage -> FilePath -> Int -> Maybe DString -> IO ()
loopWithBatPath sendMessage batPath = fix \loop consecFailCount prevRendered -> do
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
      loop (consecFailCount+1) prevRendered
    Just p -> do
      let rendered = renderBattery p
      sendMessage $ MPRendered $ Just rendered
      sleep
      loop 0 (Just $ fg (sRGB24read "#FF0000") rendered)

mainLoop :: SendMessage -> IO ()
mainLoop sendMessage = fix \loop -> do
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
