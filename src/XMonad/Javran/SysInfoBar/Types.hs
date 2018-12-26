{-# LANGUAGE ExistentialQuantification, TypeFamilies, ScopedTypeVariables #-}
module XMonad.Javran.SysInfoBar.Types where

import qualified Data.Map.Strict as M
import Data.Typeable
import Control.Concurrent.MVar
import System.Dzen

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
  -- N.B. the distinction between WState and WStateRep is necessary
  -- to ensure the injectivity
  data WState w :: *
  type WStateRep w :: *
  runWorker :: forall p. p w -> MVar BarState -> IO ()
  getWorkerState :: SomeWorkerState -> Maybe (WState w)
  getWorkerState (SomeWorkerState s) = cast s
  getStateRep :: WState w -> WStateRep w

  extractWState :: BarState -> Maybe (WState w)
  extractWState m =
    M.lookup (typeRep (Proxy :: Proxy w)) m >>=
    getWorkerState

  extractWStateRep :: forall p. p w -> BarState -> Maybe (WStateRep w)
  extractWStateRep _ m =
    getStateRep <$> (extractWState m :: Maybe (WState w))

data SomeWorkerState = forall w. Worker w => SomeWorkerState (WState w)

class Worker w => RenderableWorker w where
  -- the idea is that we always output DString,
  -- which avoids any complication
  -- involved with Printer
  wRender :: forall p. p w -> WStateRep w -> DString
