{-# LANGUAGE DeriveFunctor, TypeApplications, RecordWildCards #-}
module XMonad.Javran.SystemInfoBar where

import System.IO
import Data.Function
import Data.Char
import Control.Monad
import Text.ParserCombinators.ReadP

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
  } deriving (Functor, Show)

parseRow :: String -> ((String, CpuStatRow Int), String)
parseRow raw = case readP_to_S parseLineP raw of
    [(r, [])] -> r
    _ -> error "parse error"
  where
    cpuDesc = munch1 (not . isSpace)
    getInt = read @Int <$> munch1 isDigit
    dataPart = do
      [user, nice, system, idle, ioWait, irq, softIrq, steal] <-
        replicateM 8 (skipSpaces >> getInt)
      pure CpuStatRow {..}
    parseLineP = (,) <$> ((,) <$> cpuDesc <*> dataPart) <*> munch (const True)

getCpuStatRaw :: IO [(String, CpuStatRow Int)]
getCpuStatRaw = do
  cpuRawLines <- withFile "/proc/stat" ReadMode $ \handle -> fix $ \kont -> do
    raw <- hGetLine handle
    if take 3 raw == "cpu"
      then (raw :) <$> kont
      else pure []
  -- first "cpu ..." line is the total across all CPUs so we ignore it.
  -- assume no parsing error, but we'll like to have leftover input just in case.
  pure (map (fst . parseRow) $ drop 1 cpuRawLines)

main :: IO ()
main = getCpuStatRaw >>= print
