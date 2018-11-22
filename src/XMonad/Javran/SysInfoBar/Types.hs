{-# LANGUAGE ExistentialQuantification, TypeFamilies #-}
module XMonad.Javran.SysInfoBar.Types where

import qualified Data.Map.Strict as M
import Control.Concurrent.MVar
{-
  assume we have one to one relation between worker and running threads
  we'll allow every worker to update a globally share MVar containing BarState.
  and every worker is supposed to update its own parts indicated by a unique id
  (WorkerUniq)
-}

type WorkerUniq = String
type BarState = M.Map WorkerUniq SomeWorkerState

class Worker w where
  type WorkerState w :: *
  wUniq :: forall p. p w -> WorkerUniq
  runWorker :: forall p. p w -> MVar BarState -> IO ()

data SomeWorkerState = forall w . Worker w => SomeWorkerState w
