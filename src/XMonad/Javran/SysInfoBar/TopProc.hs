{-# LANGUAGE TypeFamilies, LambdaCase, OverloadedStrings #-}
module XMonad.Javran.SysInfoBar.TopProc
  ( TopProc
  ) where

import XMonad.Javran.SysInfoBar.Types
import Data.Typeable
import Control.Concurrent
import qualified Data.Map.Strict as M
import System.Process
import Control.Monad
import System.IO
import Data.Maybe
import Data.String

runWorkerWith :: MVar BarState -> IO ()
runWorkerWith mv = forever $ do
  let cp =
        (shell "ps --sort=-pcpu -eo comm | head -n 2")
          { std_in = NoStream
          , std_out = CreatePipe
          , std_err = Inherit
          }
  (_, Just hOut, _, _) <- createProcess cp
  (_:procNames) <- lines <$> hGetContents hOut
  let k = typeRep (Proxy :: Proxy TopProc)
      res = SomeWorkerState (St (listToMaybe procNames))
  modifyMVar_ mv (pure . M.insert k res)
  threadDelay 500000
  
data TopProc

instance Worker TopProc where
  data WState TopProc = St (Maybe String)
  type WStateRep TopProc = Maybe String
  runWorker _ = runWorkerWith
  getStateRep (St x) = x

instance RenderableWorker TopProc where
  wRender _ = \case
    Nothing -> "------"
    Just xs | length xs <= 6 -> fromString $ take 6 (xs ++ repeat ' ')
    Just xs -> fromString $ take 4 xs ++ ".."
