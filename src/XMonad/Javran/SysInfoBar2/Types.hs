module XMonad.Javran.SysInfoBar2.Types where

import Data.Time.Clock
import Data.Proxy
import Control.Concurrent

import qualified Data.Sequence as Seq
import qualified System.Dzen as Dz

data MessagePayload
  = -- for worker to send the (lazily) rendered part.
    -- this also serves as a heartbeat if we send nothing.
    MPRendered (Maybe Dz.DString)
  | -- for worker to indicate that there's an exception
    -- that cannot be recovered by itself.
    MPException String

type MessageQueue = Seq.Seq (ThreadId, (UTCTime, MessagePayload))

-- main thread will provide such a callback for sending messages.
type SendMessage = MessagePayload -> IO ()

class Worker w where
  -- interface to start the worker
  workerStart :: Proxy w -> SendMessage -> IO ()
  -- if worker doesn't send a message within this time limit,
  -- it's considered dead therefore killed & restarted
  workerDeadline :: Proxy w -> Int
