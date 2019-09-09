{-# LANGUAGE ExplicitForAll #-}
module XMonad.Javran.SysInfoBar2.Types where

import Control.Concurrent
import Data.Time.Clock

import qualified Data.Sequence as Seq
import qualified System.Dzen as Dz

newtype MessagePayload
  = {-
      for worker to send the (lazily) rendered part.
      note that sending message counts as a heartbeat,
      so even if there is no update, sending the old message is still necessary.
     -}
    MPRendered (Maybe Dz.DString)

type MessageQueue = Seq.Seq (ThreadId, (UTCTime, MessagePayload))

-- main thread will provide such a callback for sending messages.
type SendMessage = MessagePayload -> IO ()

class Worker w where
  -- interface to start the worker
  workerStart :: forall p. p w -> SendMessage -> IO ()
  -- if worker doesn't send a message within this time limit,
  -- it's considered dead therefore killed & restarted
  -- the unit is second.
  workerDeadline :: forall p. p w -> Int
