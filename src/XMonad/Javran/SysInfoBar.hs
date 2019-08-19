{-# LANGUAGE
    ScopedTypeVariables
  , ExistentialQuantification
  , FlexibleContexts
  , ConstraintKinds
  , OverloadedStrings
  , LambdaCase
  , RankNTypes
  #-}
module XMonad.Javran.SysInfoBar where

import Data.Typeable
import Control.Concurrent
import Control.Monad
import Data.Maybe
import System.Process
import System.IO

import Data.Default
import qualified System.Dzen as Dz

import XMonad.Javran.SysInfoBar.Types
import XMonad.Javran.SysInfoBar.CpuUsage (CpuUsage)
import XMonad.Javran.SysInfoBar.CpuMaxFreq (CpuMaxFreq)
import XMonad.Javran.SysInfoBar.MemUsage (MemUsage)
import XMonad.Javran.SysInfoBar.TopProc (TopProc)
import XMonad.Javran.SysInfoBar.NetStat (NetStat)
import XMonad.Javran.SysInfoBar.Mail (Mail)
import XMonad.Javran.SysInfoBar.Mpd (Mpd)
import XMonad.Javran.SysInfoBar.Battery (Battery)
import XMonad.Javran.SysInfoBar.DateTime (DateTime)

import XMonad.Javran.SysInfoBar.DzenRender (render)

{-
  system info we are looking at:

  - CPU usage for each individual ones

    + http://e2e.ti.com/support/legacy_forums/embedded/linux/f/354/t/221192

  - CPU freq

    + conky uses "freq_q" to show cpu freq in GHz, when number is omitted,
      CPU #1 is shown. here we prefer showing the maximum GHz across all available CPUs

  - memory usage

    + /proc/meminfo: seems to be just (MemTotal - MemAvailable) / MemTotal
    + MemFree is unused physical RAM, while MemAvailable is available memory
      if we were to start a new app - which tends to be larger
      I'm only interested in percentage of memory that are being actually used.

  - network Rx & Tx
  - mail checker
  - mpd state
  - whether battery is charging & battery remaining
  - date & time
  - top proc: process with top cpu utilization
 -}
data EWorker = forall w. Worker w => EW (Proxy w)

{-
  TODO: potental improvements

  - now I feel this use case is not a very good use of existential types,
    as we still have many boilerplate to write and in addition,
    we don't get much since all plugins' interfaces are the same.

 - switch to async tasks. I feel that way we can get a bit more control.

 -}
workers :: [EWorker]
workers =
  [ EW (Proxy :: Proxy CpuUsage)
  , EW (Proxy :: Proxy CpuMaxFreq)
  , EW (Proxy :: Proxy MemUsage)
  , EW (Proxy :: Proxy TopProc)
  , EW (Proxy :: Proxy NetStat)
  , EW (Proxy :: Proxy Mail)
  , EW (Proxy :: Proxy Mpd)
  , EW (Proxy :: Proxy Battery)
  , EW (Proxy :: Proxy DateTime)
  ]

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
          , "-h", "24"
          , "-fn", "DejaVu Sans Mono:pixelsize=15:antialias=true"
          , "-bg", "#505050"
          ]

main :: IO ()
main = do
    (hOut, _) <- spawnDzen
    mSt <- newMVar def
    mapM_ (forkIO . (\(EW wt) -> runWorker wt mSt)) workers
    forever $ do
      threadDelay 500000
      tryReadMVar mSt >>= \case
        Just m ->
          let renderWidget :: EWorker -> Maybe Dz.DString
              renderWidget (EW p) = render p <$> extractWStateRep p m
              rendered :: Dz.DString
              rendered =
                  foldr (\x xs -> x <> " " <> xs) mempty
                . mapMaybe renderWidget
                $ workers
          in hPutStrLn hOut (Dz.toString rendered)
        _ -> pure ()
