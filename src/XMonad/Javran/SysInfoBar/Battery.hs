{-# LANGUAGE ScopedTypeVariables, LambdaCase, TypeFamilies, OverloadedStrings #-}
module XMonad.Javran.SysInfoBar.Battery
  ( Battery
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
import XMonad.Javran.SysInfoBar.Types
import Data.Typeable
import Control.Concurrent
import qualified Data.Map.Strict as M

type BatState = (Int, Bool) -- (<capacity>, <isCharging?>)

data Battery

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

runWorkerWith :: MVar BarState -> IO ()
runWorkerWith mv = do
    -- determine battery path on startup and then lock on it.
    mBatPath <- getBatteryPath
    case mBatPath of
      Nothing -> pure ()
      Just batPath -> fix $ \run -> do
        let getBatStat :: IO (Maybe BatState)
            getBatStat = do
              c <- read . head . lines <$> readFile (batPath ++ "/capacity")
              rawS <- readFile (batPath ++ "/status")
              pure (Just (c, rawS /= "Discharging\n" && rawS /= "Not charging\n"))
        r <- catch getBatStat (handleExc Nothing)
        case r of
          Nothing -> pure ()
          Just p ->
            let k = typeRep (Proxy :: Proxy Battery)
                res = SomeWorkerState (St p)
            in modifyMVar_ mv (pure . M.insert k res)
        threadDelay 1000000
        run

instance Worker Battery where
  data WState Battery = St BatState
  type WStateRep Battery = BatState
  runWorker _ = runWorkerWith
  getStateRep (St x) = x
