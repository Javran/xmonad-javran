module XMonad.Javran.SysInfoBar2.TopProc
  ( TopProc
  ) where

import Control.Concurrent
import Control.Monad
import Data.Maybe
import System.IO
import System.Process

import XMonad.Javran.SysInfoBar2.Types
import XMonad.Javran.SysInfoBar.DzenRender (renderTopProc)

data TopProc

instance Worker TopProc where
  workerStart _ sendMessage = forever $ do
    let cp =
          (shell "ps --sort=-pcpu -eo comm | head -n 2")
            { std_in = NoStream
            , std_out = CreatePipe
            , std_err = Inherit
          }
    (_, Just hOut, _, _) <- createProcess cp
    (_:procNames) <- lines <$> hGetContents hOut
    sendMessage $ MPRendered $
      Just $ renderTopProc (listToMaybe procNames)
    threadDelay 500000

  workerDeadline _ = 5
