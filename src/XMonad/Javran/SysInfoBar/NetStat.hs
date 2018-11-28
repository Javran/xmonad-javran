{-# LANGUAGE TypeApplications #-}
module XMonad.Javran.SysInfoBar.NetStat
  ( NetStatWorker
  ) where

import System.IO
import Data.Function
import Data.Char
import Control.Monad
import Text.ParserCombinators.ReadP
import Control.Concurrent
import Data.Time.Clock
import XMonad.Javran.SysInfoBar.Types
import Data.Typeable
import qualified Data.Map.Strict as M
import Data.Monoid
import Control.Lens
import Data.Coerce

{-
  refs:
  - https://github.com/brndnmtthws/conky/blob/fcbc5c8ba2a9546f50661a4846aef2f76f07a066/src/linux.cc
    * see update_net_state
  - https://linux.die.net/man/5/proc
-}

type NetInfo = (Int, Int) -- (Rx, Tx)

parseNetStat :: String -> NetInfo
parseNetStat =
      coerce
    . foldMap parseRawLine
    . drop 2 -- first 2 lines are headers
    . lines
  where
    parseRawLine :: String -> (Sum Int, Sum Int)
    parseRawLine raw = case readP_to_S parse raw of
        [(r, [])] -> coerce r
        _ -> mempty
    parse :: ReadP (Int, Int)
    parse =
        skipSpaces *>
        munch1 (/= ':') *> skipSpaces *>
        ((munch1 isDigit `sepBy1` skipSpaces) >>= getInfo) <* skipSpaces <* eof
    getInfo :: [String] -> ReadP (Int, Int)
    getInfo [ rBytes, _rPackets, _rErrs, _rDrop, _rFifo, _rFrame, _rCompressed, _rMulticase
            , tBytes, _tPackets, _tErrs, _tDrop, _tFifo, _tColls, _tCarrier, _tCompressed
            ] = pure (read rBytes, read tBytes)
    getInfo _ = fail "parse error"

getRxTxInfo :: IO NetInfo
getRxTxInfo = parseNetStat <$> readFile "/proc/net/dev"

data NetStatWorker
