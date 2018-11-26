{-# LANGUAGE TypeFamilies #-}
module XMonad.Javran.SysInfoBar.CpuMaxFreq
  ( CpuMaxFreqWorker
  ) where

import Data.Maybe
import Data.Char
import Text.ParserCombinators.ReadP
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import XMonad.Javran.SysInfoBar.Types
import Control.Concurrent
import Data.Typeable
import Data.Function

getCpuFreqs :: IO [Double]
getCpuFreqs = mapMaybe parseLine . lines <$> readFile "/proc/cpuinfo"
  where
    parseLine :: String -> Maybe Double
    parseLine raw = case readP_to_S parse raw of
      [(r, [])] -> Just r
      _ -> Nothing
    parse :: ReadP Double
    parse =
      string "cpu MHz" *> skipSpaces *>
      char ':' *> skipSpaces *>
      -- read should be safe because ReadP is a MonadFail
      (read <$> munch1 (not . isSpace)) <* skipSpaces <* eof

getCpuMaxFreqGHz :: IO (Maybe Double)
getCpuMaxFreqGHz = fmap ((/1000) . maximum) . NE.nonEmpty <$> getCpuFreqs

runWorkerWith :: MVar BarState -> IO ()
runWorkerWith mv = fix $ \run -> do
  v <- getCpuMaxFreqGHz
  let k = typeRep (Proxy :: Proxy CpuMaxFreqWorker)  
  modifyMVar_ mv (pure . M.insert k (SomeWorkerState (CpuMaxFreqSt v)))
  threadDelay 1000000
  run

data CpuMaxFreqWorker

instance Worker CpuMaxFreqWorker where
  data WState CpuMaxFreqWorker = CpuMaxFreqSt (Maybe Double)
  type WStateRep CpuMaxFreqWorker = Maybe Double
  runWorker _ = runWorkerWith
  getStateRep (CpuMaxFreqSt v) = v
