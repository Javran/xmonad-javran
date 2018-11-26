{-# LANGUAGE ExistentialQuantification, TypeFamilies #-}
module XMonad.Javran.SysInfoBar.Types where

import qualified Data.Map.Strict as M
import Data.Typeable
import Control.Concurrent.MVar
{-
  we have one to one relation between worker and running threads
  (established by making sure there's at most one thread for each
  different type of worker and by the uniqueness of TypeRep)
  we'll allow every worker to update a globally share MVar containing BarState.
  and every worker is supposed to update its own parts in this MVar
-}

type WorkerUniq = String
type BarState = M.Map TypeRep SomeWorkerState

class Typeable w => Worker w where
  -- WState is the internal state shared in MVar
  -- WStateRep contains all info necessary for rendering
  data WState w :: *
  type WStateRep w :: *
  runWorker :: forall p. p w -> MVar BarState -> IO ()
  getWorkerState :: SomeWorkerState -> Maybe (WState w)
  getWorkerState (SomeWorkerState s) = cast s
  getStateRep :: WState w -> WStateRep w

data SomeWorkerState = forall w. Worker w => SomeWorkerState (WState w)