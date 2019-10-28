{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  , RecordWildCards
  , DeriveFunctor
  #-}
module XMonad.Javran.SysInfoBar.CpuUsage
  ( CpuUsage
  ) where

import Control.Concurrent
import Control.Monad
import Data.Char
import Data.Colour.Names
import Data.Function
import Data.String
import Data.Time.Clock
import System.Dzen
import System.IO
import Text.ParserCombinators.ReadP

import XMonad.Javran.SysInfoBar.Types

data CpuUsage

renderCpuUsage :: [Int] -> DString
renderCpuUsage xs = "[" <> foldMap rdr xs <> "]"
  where
    rdr :: Int -> DString
    rdr n
      | n >= 9 = fg red (if n > 9 then "X" else "9")
      | n >= 5 = fg orange (fromString (show n))
      | otherwise = fromString (show n)

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

getCpuStatRaw :: IO ([CpuStatRow Int], UTCTime)
getCpuStatRaw = do
  (cpuRawLines, t) <- withFile "/proc/stat" ReadMode $ \handle -> do
    -- get timestamp immediately after the proc stat file is opened,
    -- by doing so we can make sure we have the accurate time
    t <- getCurrentTime
    xs <- fix $ \kont -> do
        raw <- hGetLine handle
        if take 3 raw == "cpu"
          then (raw :) <$> kont
          else pure []
    pure (xs, t)
  -- first "cpu ..." line is the total across all CPUs so we ignore it.
  -- assume no parsing error, but we'll like to have leftover input just in case.
  -- also, hopefully we always get lists of the same length, so we don't really
  -- need to return cpu tags
  pure (map (snd . fst . parseRow) $ drop 1 cpuRawLines, t)

-- reference from conky:
-- https://github.com/brndnmtthws/conky/blob/f8ff46c2dca4d639c9287790c35999bbaae56010/src/linux.cc#L969-L975
computeCpuUsage :: CpuStatRow Int -> CpuStatRow Int -> Double
computeCpuUsage before after = fI (100 * activeTime) / fI total
  where
    fI = fromIntegral @Int @Double
    diffOn prj = prj after - prj before
    ioWaitCnt =
      let diff = diffOn ioWait -- according to doc ioWait has a chance of *decreasing*
      in if diff < 0 then 0 else diff
    idleCnt = diffOn idle
    total =
        diffOn user
      + diffOn nice
      + diffOn system
      + idleCnt
      + ioWaitCnt
      + diffOn irq
      + diffOn softIrq
      + diffOn steal
    activeTime = total - idleCnt - ioWaitCnt

startLoop :: (MessagePayload -> IO ()) -> [CpuStatRow Int] -> IO ()
startLoop sendMessage = fix $ \loop oldS -> do
    threadDelay 1000000
    (s, _) <- getCpuStatRaw
    let rendered =
          renderCpuUsage (simpl <$> zipWith computeCpuUsage oldS s)
    sendMessage (MPRendered (Just rendered))
    loop s
  where
    simpl :: Double -> Int
    simpl x
      | x >= 100 = 10
      | x <= 0   = 0
      | otherwise = floor (x/10)

instance Worker CpuUsage where
  workerStart _ sendMessage = do
    (s, _) <- getCpuStatRaw
    startLoop sendMessage s

  workerDeadline _ = 10
