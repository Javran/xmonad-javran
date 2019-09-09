{-# LANGUAGE
    OverloadedStrings
  , LambdaCase
  #-}
module XMonad.Javran.SysInfoBar2.CpuMaxFreq
  ( CpuMaxFreq
  ) where

import Control.Concurrent
import Control.Monad
import Data.Char
import Data.Maybe
import Data.String
import System.Dzen
import Text.ParserCombinators.ReadP
import Text.Printf

import qualified Data.List.NonEmpty as NE

import XMonad.Javran.SysInfoBar2.Types

renderCpuMaxFreq :: Maybe Double -> DString
renderCpuMaxFreq = \case
    Nothing -> "????GHz"
    Just d ->
      let content :: String
          content = printf "%4.2fGHz" d
      in fromString content

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

data CpuMaxFreq

instance Worker CpuMaxFreq where
  workerStart _ sendMessage = forever $ do
    v <- getCpuMaxFreqGHz
    let rendered = renderCpuMaxFreq v
    sendMessage (MPRendered (Just rendered))
    threadDelay 1000000

  workerDeadline _ = 5
