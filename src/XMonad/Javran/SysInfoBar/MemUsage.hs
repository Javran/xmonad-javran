{-# LANGUAGE
    RecordWildCards
  , TypeApplications
  , MultiWayIf
  , OverloadedStrings
  #-}
module XMonad.Javran.SysInfoBar.MemUsage
  ( MemUsage
  ) where

import Control.Concurrent
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import Data.Char
import Data.String
import Data.Word
import System.Dzen
import System.IO
import Text.ParserCombinators.ReadP
import Text.Printf

import qualified Data.ByteString.Char8 as BSC

import XMonad.Javran.SysInfoBar.Types
import XMonad.Javran.SysInfoBar.ProcParser

renderMemUsage :: (Word64, Word64) -> DString
renderMemUsage (numer, denom) = fromString ("M:" ++ msg)
  where
    fI = fromIntegral @_ @Double
    msg :: String
    msg = if
        | numer > denom -> "ERR"
        | numer == denom -> "##%" -- fully occupied
        | numer < 0 -> "ERR"
        | otherwise ->
            let pc = fI numer * 100 / fI denom
            in printf "%2d%%" (floor pc :: Int)

{-
  ref: <linux kernel source>/fs/meminfo.c
-}

data MemInfoRaw = MemInfoRaw
  { mTotal :: Word64
  , mFree :: Word64
  , mAvailable :: Word64
  }

getMemInfoRaw :: IO MemInfoRaw
getMemInfoRaw = do
  raw <- BSC.readFile "/proc/meminfo"
  let Right (vTotal, vFree, vAvailable) = parseOnly procMemInfoP raw
  pure $ MemInfoRaw vTotal vFree vAvailable

data MemUsage

instance Worker MemUsage where
  workerStart _ sendMessage = forever $ do
    MemInfoRaw {..} <- getMemInfoRaw
    let numer = mTotal - mAvailable
        denom = mTotal
    sendMessage (MPRendered (Just (renderMemUsage (numer, denom))))
    threadDelay 1000000

  workerDeadline _ = 5
