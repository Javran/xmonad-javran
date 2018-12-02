{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, FlexibleContexts, ConstraintKinds #-}
module XMonad.Javran.SysInfoBar where

import XMonad.Javran.SysInfoBar.Types
import XMonad.Javran.SysInfoBar.CpuUsage
import XMonad.Javran.SysInfoBar.MemUsage
import XMonad.Javran.SysInfoBar.CpuMaxFreq
import XMonad.Javran.SysInfoBar.DateTime
import XMonad.Javran.SysInfoBar.NetStat
import XMonad.Javran.SysInfoBar.Mpd
import XMonad.Javran.SysInfoBar.Battery
import XMonad.Javran.SysInfoBar.Mail
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
  - [x] mail checker
  - [x] mpd state
  - [x] whether battery is charging & battery remaining
  - [x] date & time
 -}
data EWorker = forall w. PrintableWorker w => EWorker (Proxy w)

type PrintableWorker w = (Worker w, Show (WStateRep w))

workers :: [EWorker]
workers =
  [ EWorker (Proxy :: Proxy CpuUsage)
  , EWorker (Proxy :: Proxy MemUsage)
  , EWorker (Proxy :: Proxy CpuMaxFreq)
  , EWorker (Proxy :: Proxy NetStat)
  , EWorker (Proxy :: Proxy DateTime)
  , EWorker (Proxy :: Proxy Mpd)
  , EWorker (Proxy :: Proxy Battery)
  , EWorker (Proxy :: Proxy Mail)
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
