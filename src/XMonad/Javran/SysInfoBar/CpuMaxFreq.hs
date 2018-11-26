module XMonad.Javran.SysInfoBar.CpuMaxFreq
  (
  ) where

import Data.Maybe
import Data.Char
import Text.ParserCombinators.ReadP
import Data.Semigroup
import Control.Monad
import qualified Data.List.NonEmpty as NE

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

getCpuMaxFreq :: IO (Maybe (Max Double))
getCpuMaxFreq = ((Just . sconcat . fmap Max) <=< NE.nonEmpty) <$> getCpuFreqs
