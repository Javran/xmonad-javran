{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module XMonad.Javran.SysInfoBar.Battery
  (
  ) where

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

import Control.Exception
import Control.Monad
import Data.Function
import System.Directory

data BatteryWorker

sysPowerSupply :: FilePath
sysPowerSupply = "/sys/class/power_supply/"

getBatteryPath :: IO (Maybe FilePath)
getBatteryPath = catch tryGet (handleExc Nothing)
  where
    handleExc :: a -> IOException -> IO a
    handleExc x _ =  pure x

    testProperBatDir :: FilePath -> IO (Maybe FilePath)
    testProperBatDir sub = catch doTest (handleExc Nothing)
      where
        doTest = do
          guard (take 3 sub == "BAT")
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
