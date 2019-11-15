{-# LANGUAGE
    DeriveGeneric
  , DeriveAnyClass
  , DeriveFunctor
  , RecordWildCards
  , OverloadedStrings
  #-}
module XMonad.Javran.SysInfoBar.ProcParser
  ( CpuStatRow(..)
  , ParsedCpuStatRow
  , NetDevStat(..)
  , procStatP
  , procNetDevP
  , procCpuInfoP
  , procMemInfoP

  ) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Attoparsec.ByteString.Char8 as P
import Data.Scientific
import Data.Word
import System.IO
import GHC.Generics
import Control.DeepSeq

import qualified Data.ByteString.Char8 as BSC

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
  } deriving (Show, Generic, NFData, Functor)

-- return type: (<cpu id>, (<parsed>, <leftovers of that line>))
type ParsedCpuStatRow = (Maybe Word8, (CpuStatRow Word64, BSC.ByteString))

-- consume rest of the current line, '\n' is also consumed but removed from the result.
restOfCurrentLine :: Parser BSC.ByteString
restOfCurrentLine = P.takeWhile (/= '\n') <* anyChar -- this one must be newline, no check necessary.

cpuStatRowP :: Bool -> Parser ParsedCpuStatRow
cpuStatRowP isSummaryRow = do
  _ <- "cpu"
  mCpuId <- if isSummaryRow
    then pure Nothing
    else Just <$> decimal
  [user, nice, system, idle, ioWait, irq, softIrq, steal] <-
    replicateM 8 (skipSpace >> decimal)
  leftover <- restOfCurrentLine
  pure (mCpuId, (CpuStatRow {..}, leftover))

procStatP :: Parser (ParsedCpuStatRow, [ParsedCpuStatRow])
procStatP = do
  cpuSummary <- cpuStatRowP True
  xs <- many1 (cpuStatRowP False)
  pure (cpuSummary, xs)

data NetDevStat a
  = NetDevStat
  { ndRxBytes :: a
  , ndRxPackets :: a
  , ndRxErrs :: a
  , ndRxDrop :: a
  , ndRxFifo :: a
  , ndRxFrame :: a
  , ndRxCompressed :: a
  , ndRxMulticast :: a
  , ndTxBytes :: a
  , ndTxPackets :: a
  , ndTxErrs :: a
  , ndTxDrop :: a
  , ndTxFifo :: a
  , ndTxColls :: a
  , ndTxCarrier :: a
  , ndTxCompressed :: a
  } deriving (Show, Generic, NFData)

-- kernel source: net/core/net-procfs.c
procNetDevP :: Parser [(BSC.ByteString, NetDevStat Word64)]
procNetDevP = do
    -- skip first two lines which are hard-coded header.
    restOfCurrentLine
    restOfCurrentLine
    many1 ifLine
  where
    ifLine = do
      skipSpace
      -- https://git.kernel.org/pub/scm/network/iproute2/iproute2.git/tree/lib/utils.c?id=1f420318bda3cc62156e89e1b56d60cc744b48ad#n827
      ifName <- P.takeWhile (\c -> not (c == ':' || c == '\\' || isSpace c))
      skipSpace
      ":"
      [ ndRxBytes, ndRxPackets, ndRxErrs, ndRxDrop
        , ndRxFifo, ndRxFrame, ndRxCompressed, ndRxMulticast
        , ndTxBytes, ndTxPackets, ndTxErrs, ndTxDrop
        , ndTxFifo, ndTxColls, ndTxCarrier, ndTxCompressed
        ] <- replicateM 16 (skipSpace *> decimal)
      -- the table is hard-coded, so I'd prefer to be picky and insists that it ends right here.
      "\n"
      pure (ifName, NetDevStat {..})

procCpuInfoP :: Parser [Scientific]
procCpuInfoP =
    catMaybes <$>
      many1 ((Just <$> parseCpuFreqLine) <|> (Nothing <$ restOfCurrentLine))
  where
    parseCpuFreqLine =
      "cpu MHz" >> skipSpace
      >> ":" >> skipSpace
      >> P.scientific <* restOfCurrentLine

procMemInfoP :: Parser (Word64, Word64, Word64)
procMemInfoP =
    (,,)
      <$> parseRowKb "MemTotal:"
      <*> parseRowKb "MemFree:"
      <*> parseRowKb "MemAvailable:"
  where
    parseRowKb fieldNameP =
      fieldNameP *> skipSpace *> decimal <* " kB\n"
