{-# LANGUAGE TypeFamilies #-}
module XMonad.Javran.SysInfoBar.NetStat
  ( NetStat
  ) where

import Data.Char
import Text.ParserCombinators.ReadP
import Control.Concurrent
import XMonad.Javran.SysInfoBar.Types
import Data.Typeable
import qualified Data.Map.Strict as M
import Data.Monoid
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
    parse :: ReadP (Sum Int, Sum Int)
    parse = do
        skipSpaces
        ifn <- munch1 (/= ':')
        case ifn of
          -- ignore loopback interface
          "lo" -> pure mempty
          _ ->
            char ':' *> skipSpaces *>
            ((munch1 isDigit `sepBy1` skipSpaces) >>= getInfo) <* skipSpaces
    getInfo [ rBytes, _rPackets, _rErrs, _rDrop, _rFifo, _rFrame, _rCompressed, _rMulticase
            , tBytes, _tPackets, _tErrs, _tDrop, _tFifo, _tColls, _tCarrier, _tCompressed
            ] = pure (Sum $ read rBytes, Sum $ read tBytes)
    getInfo _ = fail "parse error"

getRxTxInfo :: IO NetInfo
getRxTxInfo = parseNetStat <$> readFile "/proc/net/dev"

runWorkerWith :: MVar BarState -> IO ()
runWorkerWith mv = getRxTxInfo >>= run
  where
    run (oldRx, oldTx) = do
      threadDelay 1000000
      p@(rx, tx) <- getRxTxInfo
      let res = SomeWorkerState $ St (rx - oldRx, tx - oldTx)
          k = typeRep (Proxy :: Proxy NetStat)
      modifyMVar_ mv (pure . M.insert k res)
      run p

data NetStat

instance Worker NetStat where
  data WState NetStat = St NetInfo
  type WStateRep NetStat = NetInfo
  runWorker _ = runWorkerWith
  getStateRep (St s) = s
