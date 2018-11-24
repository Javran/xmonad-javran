{-# LANGUAGE ScopedTypeVariables #-}
module XMonad.Javran.SysInfoBar where

import XMonad.Javran.SysInfoBar.Types
import XMonad.Javran.SysInfoBar.CpuUsage
import XMonad.Javran.SysInfoBar.MemUsage
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

  - [ ] CPU freq

    + TODO: now that every CPU can have an dividual freq, how does conky handle that
      and what should we do with that?

  - [x] memory usage

    + /proc/meminfo: seems to be just (MemTotal - MemAvailable) / MemTotal
    + MemFree is unused physical RAM, while MemAvailable is available memory
      if we were to start a new app - which tends to be larger
      I'm only interested in percentage of memory that are being actually used.

  - [ ] network Rx & Tx
  - [ ] mail checker
  - [ ] mpd state
  - [ ] whether battery is charging
  - [ ] battery remaining
  - [ ] date
  - [ ] time

  + stage 1 is to grab these info in a constant interval (say 1 sec)
  + stage 2 is to have a process working on this, and impl another component
    to render things in dzen-format

 -}

main :: IO ()
main = do
    mSt <- newMVar def
    _ <- forkIO $ runWorker (Proxy :: Proxy CpuUsageWorker) mSt
    _ <- forkIO $ runWorker (Proxy :: Proxy MemUsageWorker) mSt
    fix $ \run -> do
      threadDelay 500000
      mv <- tryReadMVar mSt
      putStrLn "CpuUsage:"
      case mv of
        Just m
          | k <- typeRep (Proxy :: Proxy CpuUsageWorker)
          , (Just s) <- M.lookup k m
          , (Just (s' :: WState CpuUsageWorker)) <- getWorkerState s
            ->
            print (getStateRep s')
        _ -> putStrLn "<empty>"
      putStrLn "MemUsage:"
      case mv of
        Just m
          | k <- typeRep (Proxy :: Proxy MemUsageWorker)
          , (Just s) <- M.lookup k m
          , (Just (s' :: WState MemUsageWorker)) <- getWorkerState s
            ->
            print (getStateRep s')
        _ -> putStrLn "<empty>"
      run
