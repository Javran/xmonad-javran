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
import Data.Colour.SRGB
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

{-
  TODO: eliminate RenderableWorker

  - this is not the right thing to do: Worker are supposed to
    get info from the system and that's it. how it should be presented
    to the user isn't Worker's work.
  - we are lucky that TypeRep is a class of Eq, which means user can take a WStateRep
    and its corresponding Worker type and do different things accordingly - and that
    got to be a better solution.
 -}

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
  - [x] top proc: process with top cpu utilization
 -}
data EWorker = forall w. RenderableWorker w => EW (Proxy w)

type PrintableWorker w = (Worker w, Show (WStateRep w))

workerSpecs :: [(EWorker, Dz.DString -> Dz.DString)]
workerSpecs =
  [ (EW (Proxy :: Proxy CpuUsage), Dz.fg (sRGB24read "#FFFF00"))
  , (EW (Proxy :: Proxy CpuMaxFreq), Dz.fg (sRGB24read "#FF80A0"))
  , (EW (Proxy :: Proxy MemUsage), Dz.fg (sRGB24read "#00FF00"))
  , (EW (Proxy :: Proxy TopProc), Dz.fg (sRGB24read "#FF00FF"))
    -- since NetStat are two small "widgets", we'll let it do its own coloring.
  , (EW (Proxy :: Proxy NetStat), id)
  , (EW (Proxy :: Proxy Mail), Dz.fg (sRGB24read "#FFFFFF"))
  , (EW (Proxy :: Proxy Mpd), Dz.fg (sRGB24read "#FF80FF"))
  , (EW (Proxy :: Proxy Battery), Dz.fg (sRGB24read "#FF8080"))
    -- same reason as that of NetStat.
  , (EW (Proxy :: Proxy DateTime), id)
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
    mapM_ (forkIO . (\(EW wt) -> runWorker wt mSt) . fst) workerSpecs
    forever $ do
      threadDelay 500000
      tryReadMVar mSt >>= \case
        Just m ->
          let renderWidget :: (EWorker, Dz.DString -> Dz.DString) -> Maybe Dz.DString
              renderWidget (EW p@(ty :: Proxy w), after) = do
                  s <- extractWStateRep p m
                  pure $ after (wRender ty s)
              rendered :: Dz.DString
              rendered =
                  foldr (\x xs -> x <> " " <> xs) mempty
                . mapMaybe renderWidget
                $ workerSpecs
          in hPutStrLn hOut (Dz.toString rendered)
        _ -> pure ()
