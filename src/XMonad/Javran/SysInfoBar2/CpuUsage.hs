module XMonad.Javran.SysInfoBar2.CpuUsage
  ( CpuUsage
  ) where

import Control.Concurrent
import Data.Function

import qualified XMonad.Javran.SysInfoBar.CpuUsage as CU

import XMonad.Javran.SysInfoBar.DzenRender (renderCpuUsage)
import XMonad.Javran.SysInfoBar2.Types

data CpuUsage

startLoop :: (MessagePayload -> IO ()) -> [CU.CpuStatRow Int] -> IO ()
startLoop sendMessage = fix $ \loop oldS -> do
    threadDelay 1000000
    (s, _) <- CU.getCpuStatRaw
    let rendered =
          renderCpuUsage (simpl <$> zipWith CU.computeCpuUsage oldS s)
    sendMessage (MPRendered (Just rendered))
    loop s
  where
    simpl :: Double -> Int
    simpl x
      | x >= 100 = 10
      | x <= 0   = 0
      | otherwise = floor (x/10)

instance Worker CpuUsage where
  workerStart _ sendMessage = do
    (s, _) <- CU.getCpuStatRaw
    startLoop sendMessage s

  workerDeadline _ = 10
