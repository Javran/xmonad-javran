{-# LANGUAGE TypeFamilies, RecordWildCards #-}
module XMonad.Javran.SysInfoBar.MemUsage
  ( MemUsage
  ) where

import System.IO
import Control.Monad
import Text.ParserCombinators.ReadP
import XMonad.Javran.SysInfoBar.Types
import Data.Char
import Data.Typeable
import Control.Concurrent
import Data.Function
import qualified Data.Map.Strict as M

{-
  ref: <linux kernel source>/fs/meminfo.c
-}

data MemInfoRaw = MemInfoRaw
  { mTotal :: Int
  , mFree :: Int
  , mAvailable :: Int
  }

getMemInfoRaw :: IO MemInfoRaw
getMemInfoRaw = do
    [rawTotal, rawFree, rawAvail] <- withFile "/proc/meminfo" ReadMode $ \handle ->
      replicateM 3 (hGetLine handle)
    let p fName raw =
          case readP_to_S (parseRawField fName) raw of
              [(v,[])] -> v
              _ -> error $ "parse error for: " ++ fName
    pure (MemInfoRaw
           (p "MemTotal" rawTotal)
           (p "MemFree" rawFree)
           (p "MemAvailable" rawAvail))
  where
    parseRawField :: String -> ReadP Int
    parseRawField fieldName =
      string fieldName >> char ':' >> skipSpaces >>
      (read <$> munch1 isDigit) <* string " kB"

runWorkerWith :: MVar BarState -> IO ()
runWorkerWith mv = fix $ \run -> do
  MemInfoRaw {..} <- getMemInfoRaw
  let numer = mTotal - mAvailable
      denom = mTotal
      k = typeRep (Proxy :: Proxy MemUsage)
      res = SomeWorkerState (St (numer, denom))
  modifyMVar_ mv (pure . M.insert k res)
  threadDelay 1000000
  run

data MemUsage deriving Typeable

instance Worker MemUsage where
  data WState MemUsage = St (Int, Int)
  type WStateRep MemUsage = (Int, Int)
  runWorker _ = runWorkerWith
  getStateRep (St x) = x
