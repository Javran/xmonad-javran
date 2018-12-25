{-# LANGUAGE
    ScopedTypeVariables
  , ExistentialQuantification
  , FlexibleContexts
  , ConstraintKinds
  , TemplateHaskell
  , OverloadedStrings
  #-}
module XMonad.Javran.SysInfoBar where

import XMonad.Javran.SysInfoBar.Types
import XMonad.Javran.SysInfoBar.CpuUsage (CpuUsage)
import XMonad.Javran.SysInfoBar.MemUsage ()
import XMonad.Javran.SysInfoBar.CpuMaxFreq (CpuMaxFreq)
import XMonad.Javran.SysInfoBar.DateTime ()
import XMonad.Javran.SysInfoBar.NetStat ()
import XMonad.Javran.SysInfoBar.Mpd ()
import XMonad.Javran.SysInfoBar.Battery ()
import XMonad.Javran.SysInfoBar.Mail ()
import Data.Default
import Data.Typeable
import Control.Concurrent
import qualified Data.Map.Strict as M
import XMonad.Javran.SysInfoBar.TH
import Control.Monad
import System.Process
import System.IO
import qualified System.Dzen as Dz
import Data.Colour.Names
{-

  this module aims at eliminating the need for
  current conky ==> StreamConverter ==> dzen pipeline,
  simplifying it into SystemInfoBar ==> dzen,
  in which we manage a separated instance of dzen.

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
data EWorker = forall w. RenderableWorker w => EWorker (Proxy w)

type PrintableWorker w = (Worker w, Show (WStateRep w))

workers :: [EWorker]
workers = [EWorker (Proxy :: Proxy CpuUsage), EWorker (Proxy :: Proxy CpuMaxFreq)]

spawnDzen :: IO (Handle, ProcessHandle)
spawnDzen = createProcess cp >>= trAndSet
  where
    trAndSet (Just hInp, _, _, hProc) = do
      hSetBuffering hInp LineBuffering
      pure (hInp, hProc)
    trAndSet _ = error "failed while trying to spawn dzen"
    cp = initCp { std_in = CreatePipe }
      where
        initCp = proc "/usr/bin/dzen2"
          [ "-w", "810"
          , "-x", "900"
          -- TODO: remove -y after done
          , "-y", "24"
          , "-h", "24"
          , "-fn", "DejaVu Sans Mono:pixelsize=15:antialias=true"
          , "-bg", "#505050"
          ]

main :: IO ()
main = do
    (hOut, _) <- spawnDzen
    mSt <- newMVar def
    mapM_ (forkIO . (\(EWorker wt) -> runWorker wt mSt)) workers
    forever $ do
      threadDelay 500000
      mv <- tryReadMVar mSt
      let uTy = Proxy :: Proxy CpuUsage
          uRep = typeRep uTy
          uTy1 = Proxy :: Proxy CpuMaxFreq
          uRep1 = typeRep uTy1
      case mv of
        Just m
          | Just s <- M.lookup uRep m
          , (Just (s' :: WState CpuUsage)) <- getWorkerState s
          , content <- Dz.fg yellow $ wRender uTy (getStateRep s')
          , Just s1 <- M.lookup uRep1 m
          , (Just (s1' :: WState CpuMaxFreq)) <- getWorkerState s1
          , content1 <- Dz.fg red $ wRender uTy1 (getStateRep s1')
            ->
              -- TODO: obviously we don't need all parts to all be available to function.
              hPutStrLn hOut (Dz.toString $ content <> " " <> content1)
        _ -> pure ()
