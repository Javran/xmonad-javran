module XMonad.Javran.SysInfoBar.CpuMaxFreq
  (
  ) where

import Data.Maybe
import Data.Char
import Text.ParserCombinators.ReadP
import Data.Semigroup

import Control.Lens

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

getCpuMaxFreq :: IO (Maybe Double)
getCpuMaxFreq =
  au (iso (fmap getMax) (Just . Max)) foldMap <$> getCpuFreqs
