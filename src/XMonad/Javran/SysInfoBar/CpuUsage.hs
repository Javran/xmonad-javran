{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  , RecordWildCards
  , DeriveFunctor
  , BlockArguments
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
import Data.Attoparsec.ByteString.Char8

import qualified Data.ByteString.Char8 as BSC

import XMonad.Javran.SysInfoBar.Types
import XMonad.Javran.SysInfoBar.ProcParser

data CpuUsage

renderCpuUsage :: [Int] -> DString
renderCpuUsage xs = "[" <> foldMap rdr xs <> "]"
  where
    rdr :: Int -> DString
    rdr n
      | n >= 9 = fg red (if n > 9 then "X" else "9")
      | n >= 5 = fg orange (fromString (show n))
      | otherwise = fromString (show n)

getCpuStatRaw :: IO ([CpuStatRow Int], UTCTime)
getCpuStatRaw = do
  t <- getCurrentTime
  raw <- BSC.readFile "/proc/stat"
  case parseOnly procStatP raw of
    Left _ -> pure ([], t)
    Right (_, rs) ->
      let convert (_, (r, _)) = fmap fromIntegral r
      in pure (convert <$> rs, t)

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
startLoop sendMessage = fix \loop oldS -> do
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
