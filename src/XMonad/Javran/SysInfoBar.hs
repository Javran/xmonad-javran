{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, FlexibleContexts, ConstraintKinds #-}
module XMonad.Javran.SysInfoBar where

import XMonad.Javran.SysInfoBar.Types
import XMonad.Javran.SysInfoBar.CpuUsage
import XMonad.Javran.SysInfoBar.MemUsage
import XMonad.Javran.SysInfoBar.CpuMaxFreq
import XMonad.Javran.SysInfoBar.DateTime
import XMonad.Javran.SysInfoBar.NetStat
import XMonad.Javran.SysInfoBar.Mpd
import Data.Default
import Data.Function
import Data.Typeable
import Control.Concurrent
import qualified Data.Map.Strict as M

{-
  WIP.

  this module aims at eliminating the need for
  current conky ==> StreamConverter ==> dzen pipeline,
  simplifying it into a single SystemInfoBar,
  in which it manages a separated instance of dzen.
-}

{-
  TODO

  system info we are looking at:

  - [x] CPU usage for each individual ones

    + http://e2e.ti.com/support/legacy_forums/embedded/linux/f/354/t/221192

  - [x] CPU freq

    + conky uses "freq_q" to show cpu freq in GHz, when number is omitted,
      CPU #1 is shown. here we prefer showing the maximum GHz across all available CPUs

  - [x] memory usage

    + /proc/meminfo: seems to be just (MemTotal - MemAvailable) / MemTotal
    + MemFree is unused physical RAM, while MemAvailable is available memory
      if we were to start a new app - which tends to be larger
      I'm only interested in percentage of memory that are being actually used.

  - [x] network Rx & Tx
  - [ ] mail checker
  - [x] mpd state
  - [ ] whether battery is charging & battery remaining

    ref: https://www.kernel.org/doc/Documentation/ABI/testing/sysfs-class-power
    - it seems reasonable to assume that we'll have at most one battery.
    - /sys/class/power_supply/<XXX>/type must be "Battery", and "BAT" is a prefix
    - /sys/class/power_supply/<XXX>/capacity: 0 ~ 100 (int)
    - /sys/class/power_supply/<XXX>/status: we just want to know whether it's charging:
      "Discharging" or "Not charging" will be negative indicators
      (Note: for some reason my laptop shows "Unknown" when power is plugged in.)

  - [x] date & time

  + stage 1 is to grab these info in a constant interval (say 1 sec)
  + stage 2 is to have a process working on this, and impl another component
    to render things in dzen-format

 -}
data EWorker = forall w. PrintableWorker w => EWorker (Proxy w)

type PrintableWorker w = (Worker w, Show (WStateRep w))

workers :: [EWorker]
workers =
  [ EWorker (Proxy :: Proxy CpuUsageWorker)
  , EWorker (Proxy :: Proxy MemUsageWorker)
  , EWorker (Proxy :: Proxy CpuMaxFreqWorker)
  , EWorker (Proxy :: Proxy NetStatWorker)
  , EWorker (Proxy :: Proxy DateTimeWorker)
  , EWorker (Proxy :: Proxy MpdWorker)
  ]

main :: IO ()
main = do
    mSt <- newMVar def
    mapM_ (forkIO . (\(EWorker wt) -> runWorker wt mSt)) workers
    fix $ \run -> do
      threadDelay 500000
      mv <- tryReadMVar mSt
      let viz :: forall w. PrintableWorker w => Proxy w -> IO ()
          viz wt = do
              putStrLn $ show (typeRep wt) ++ ":"
              case mv of
                Just m
                  | k <- typeRep wt
                  , (Just s) <- M.lookup k m
                  , (Just (s' :: WState w)) <- getWorkerState s
                    ->
                    print (getStateRep s')
                _ -> putStrLn "<empty>"
      mapM_ (\(EWorker wt) -> viz wt) workers
      run
