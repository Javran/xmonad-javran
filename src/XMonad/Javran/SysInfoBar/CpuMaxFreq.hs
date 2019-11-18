{-# LANGUAGE
    OverloadedStrings
  , LambdaCase
  , BlockArguments
  #-}
module XMonad.Javran.SysInfoBar.CpuMaxFreq
  ( CpuMaxFreq
  ) where

import Control.Concurrent
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import Data.Char
import Data.Maybe
import Data.String
import System.Dzen
import Text.Printf

import qualified Data.ByteString.Char8 as BSC
import qualified Data.List.NonEmpty as NE

import XMonad.Javran.SysInfoBar.Types
import XMonad.Javran.SysInfoBar.ProcParser

renderCpuMaxFreq :: Maybe Double -> DString
renderCpuMaxFreq = \case
    Nothing -> "????GHz"
    Just d ->
      let content :: String
          content = printf "%4.2fGHz" d
      in fromString content

getCpuFreqs :: IO [Double]
getCpuFreqs = doParse <$> BSC.readFile "/proc/cpuinfo"
  where
    doParse :: BSC.ByteString -> [Double]
    doParse raw = case parseOnly procCpuInfoP raw of
      Left _ -> []
      Right r -> realToFrac <$> r

getCpuMaxFreqGHz :: IO (Maybe Double)
getCpuMaxFreqGHz = fmap ((/1000) . maximum) . NE.nonEmpty <$> getCpuFreqs

data CpuMaxFreq

instance Worker CpuMaxFreq where
  workerStart _ sendMessage = forever do
    v <- getCpuMaxFreqGHz
    let rendered = renderCpuMaxFreq v
    sendMessage (MPRendered (Just rendered))
    threadDelay 1000000

  workerDeadline _ = 5
