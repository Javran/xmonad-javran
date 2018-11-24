module XMonad.Javran.SysInfoBar.MemUsage
  (
  ) where

import System.IO
import Control.Monad
import Text.ParserCombinators.ReadP
import Data.Char

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
      (read <$> munch1 isDigit) <* string "kB"
