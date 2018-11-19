{-# LANGUAGE DeriveFunctor #-}
module XMonad.Javran.SystemInfoBar where

import System.IO
import Data.Function

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

  - [ ] CPU usage for each individual ones

    + http://e2e.ti.com/support/legacy_forums/embedded/linux/f/354/t/221192

  - [ ] CPU freq
  - [ ] memory usage
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

data CpuStatRow a = CpuStatRow
  { user :: a
  , nice :: a
  , system :: a
  , idle :: a -- count as idle time
  , ioWait :: a -- count as idle time
  , irq :: a
  , softIrq :: a
  , steal :: a
    -- there are actually 2 extra fields called "guest" and "guest_nice"
    -- in a recent version of kernel,
    -- but no word is given on how to deal with these two fields - guess we'll just ignore them.
  } deriving (Functor)

parseRow :: String -> (CpuStatRow Int, String)
parseRow = error "TODO"

getCpuStatRaw :: IO [CpuStatRow Int]
getCpuStatRaw = do
  cpuRawLines <- withFile "/proc/stat" ReadMode $ \handle -> fix $ \kont -> do
    raw <- hGetLine handle
    if take 3 raw == "cpu"
      then (raw :) <$> kont
      else pure []
  pure (map (fst . parseRow) $ drop 1 cpuRawLines)

main :: IO ()
main = getCpuStatRaw >> pure ()
