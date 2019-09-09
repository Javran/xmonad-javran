{-# LANGUAGE
    RecordWildCards
  , TypeApplications
  , MultiWayIf
  , OverloadedStrings
  #-}
module XMonad.Javran.SysInfoBar2.MemUsage
  ( MemUsage
  ) where

import Control.Concurrent
import Control.Monad
import Data.Char
import Data.String
import System.Dzen
import System.IO
import Text.ParserCombinators.ReadP
import Text.Printf

import XMonad.Javran.SysInfoBar2.Types

renderMemUsage :: (Int, Int) -> DString
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

data MemUsage

instance Worker MemUsage where
  workerStart _ sendMessage = forever $ do
    MemInfoRaw {..} <- getMemInfoRaw
    let numer = mTotal - mAvailable
        denom = mTotal
    sendMessage (MPRendered (Just (renderMemUsage (numer, denom))))
    threadDelay 1000000

  workerDeadline _ = 5
