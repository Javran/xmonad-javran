{-# LANGUAGE TypeFamilies, LambdaCase, OverloadedStrings #-}
module XMonad.Javran.SysInfoBar.CpuMaxFreq
  ( CpuMaxFreq
  ) where

import Data.Maybe
import Data.String
import Data.Char
import Text.ParserCombinators.ReadP
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import XMonad.Javran.SysInfoBar.Types
import Control.Concurrent
import Data.Typeable
import Text.Printf
import Control.Monad

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
runWorkerWith mv = forever $ do
  v <- getCpuMaxFreqGHz
  let k = typeRep (Proxy :: Proxy CpuMaxFreq)  
  modifyMVar_ mv (pure . M.insert k (SomeWorkerState (St v)))
  threadDelay 1000000

data CpuMaxFreq

instance Worker CpuMaxFreq where
  data WState CpuMaxFreq = St (Maybe Double)
  type WStateRep CpuMaxFreq = Maybe Double
  runWorker _ = runWorkerWith
  getStateRep (St v) = v

instance RenderableWorker CpuMaxFreq where
  wRender _ = \case
    Nothing -> "????GHz"
    Just d ->
      let content :: String
          content = printf "%4.2fGHz" d
      in fromString content
