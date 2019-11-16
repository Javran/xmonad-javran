{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  #-}
module XMonad.Javran.SysInfoBar.NetStat
  ( NetStat
  ) where

import Control.Concurrent
import Data.Char
import Data.Coerce
import Data.Colour.SRGB
import Data.Monoid
import Data.String
import System.Dzen
import Data.Attoparsec.ByteString.Char8
import Data.Word

import qualified Data.ByteString.Char8 as BSC

import XMonad.Javran.Utils
import XMonad.Javran.SysInfoBar.Types
import XMonad.Javran.SysInfoBar.ProcParser

renderNetStat :: (Word64, Word64) -> DString
renderNetStat (rBytes, tBytes) =
    rContent <> " " <> tContent
  where
    rContent =
        fg (sRGB24read "#80FF80")
      . fromString
      $ "R:" ++ byteToReadableString (fromIntegral rBytes)
    tContent =
        fg (sRGB24read "#8080FF")
      . fromString
      $ "T:" ++ byteToReadableString (fromIntegral tBytes)

{-
  refs:
  - https://github.com/brndnmtthws/conky/blob/fcbc5c8ba2a9546f50661a4846aef2f76f07a066/src/linux.cc
    * see update_net_state
  - https://linux.die.net/man/5/proc
-}

type NetInfo = (Word64, Word64) -- (Rx, Tx)

parseNetStat :: BSC.ByteString -> NetInfo
parseNetStat raw = case parseOnly procNetDevP raw of
  Left _ -> (0, 0)
  Right rs ->
    let p@(Sum _, Sum _) = foldMap go rs
        go (ifName, ~NetDevStat {ndRxBytes = rx , ndTxBytes = tx}) =
          if ifName == "lo"
            then mempty
            else (Sum rx, Sum tx)
    in coerce @(Sum Word64, Sum Word64) @NetInfo p

getRxTxInfo :: IO NetInfo
getRxTxInfo = parseNetStat <$> BSC.readFile "/proc/net/dev"

data NetStat

instance Worker NetStat where
  workerStart _ sendMessage = getRxTxInfo >>= run
    where
      run (oldRx, oldTx) = do
        threadDelay 1000000
        p@(rx, tx) <- getRxTxInfo
        let rendered = renderNetStat (rx - oldRx, tx - oldTx)
        sendMessage (MPRendered (Just rendered))
        run p

  workerDeadline _ = 5
