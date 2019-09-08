{-# LANGUAGE
    ExistentialQuantification
  , FlexibleContexts
  , LambdaCase
  , RecordWildCards
  , NamedFieldPuns
  , RecursiveDo
  #-}
module XMonad.Javran.SysInfoBar2.SysInfoBar2
  ( main
  ) where

{-
  A playground for trying out idea for improved design of SysInfoBar.

  - now I feel the current use case is not a very good use of existential types,
    as we still have many boilerplate to write and in addition,
    we don't get much since all plugins' interfaces are the same.
    (also we do need to have render logic for each of them in one place -
    there is no actual point of the current isolation provided by existential type.

 - switch to async tasks. I feel that way we can get a bit more control.

 - we'll also need a common data type for processes to hold their state or
   send response back.

 -}

{-
  Design revisit:

  - there's no need of storing states in a shared dict.
    we can just let workers pass rendered messages (in the form of thunk),
    together with a timestamp. so main thread knows that a thread
    might be dead for random reasons and try to kill and restart it.

  - messages goes into a queue in main thread, which then reads and handles the actual updating
    (optimization: if multiple messages are passed from same thread, we can only take the latest one)

  - for the health check idea above to work, defined a deadline for each worker.

  - threads should really be identified with thread ids - using type as identifier
    limits us to use one thread for each type, which is not necessary.

 -}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.State.Strict
import Data.Proxy
import Data.Time.Clock
import System.IO
import System.Process

import qualified Data.Vector as V
import qualified Data.IntMap.Strict as IM
import qualified Data.Sequence as Seq

import XMonad.Javran.SysInfoBar2.Types
import XMonad.Javran.SysInfoBar2.CpuUsage (CpuUsage)
import XMonad.Javran.SysInfoBar2.DateTime (DateTime)

data EWorker = forall w. Worker w => EW (Proxy w)

-- this vector specifies stuff that we want to show
-- on the status bar in that order.
-- at runtime each element will be identified by its index here
-- and assigned a thread for doing the actual work.
-- unlike first version, duplicated elements are allowed.
workersSpec :: V.Vector EWorker
workersSpec = V.fromList
  [ EW (Proxy :: Proxy CpuUsage)
  , EW (Proxy :: Proxy DateTime)
  ]

data WorkerRep
  = WorkerRep
  { wrId :: Int
  , wrLastKnown :: UTCTime -- last time it sends a message / initialized to creation time.
  , wrAsync :: Async ()
  }

-- runtime representation of workers.
type WorkersRep = IM.IntMap WorkerRep

{-
  main thread loop:

  - handle message, and render to dzen2
  - worker maintenance: kill / restart workers if needed
  - sleep and wait for update
  - loop

 -}

spawnDzen :: IO (Handle, ProcessHandle)
spawnDzen = createProcess cp >>= trAndSet
  where
    trAndSet (Just hInp, _, _, hProc) = do
      hSetBuffering hInp LineBuffering
      pure (hInp, hProc)
    trAndSet _ = error "failed while trying to spawn dzen"
    cp = initCp { std_in = CreatePipe }
      where
        initCp = proc "/usr/bin/dzen2"
          [ "-w", "810"
          , "-x", "900"
            {-
              TODO: -y 24 is to avoid overlaping with v1 when debuging,
              should remove this when done.
             -}
          , "-y", "24"
          , "-h", "24"
          , "-fn", "DejaVu Sans Mono:pixelsize=15:antialias=true"
          , "-bg", "#505050"
          ]


mainLoop :: Handle -> MVar MessageQueue -> WorkersRep -> IO ()
mainLoop hOut mQueue = evalStateT $ forever $ do
  -- TODO: process incoming message
  q <- liftIO $ swapMVar mQueue Seq.empty
  liftIO $ putStrLn $ "Received " <> show (Seq.length q) <> " messages"
  forM_ q $ \(tId,(t,_mp)) ->
    gets ( IM.minViewWithKey
         . IM.filter (\WorkerRep{wrAsync} -> asyncThreadId wrAsync == tId)
         ) >>= \case
      Nothing -> pure () -- ignore, unknown thread. (some thread that we no longer keep record)
      Just ((wId, wr), _) ->
        -- TODO: handle payload
        modify (IM.insert wId wr{wrLastKnown=t})

  let maintainWorker wId (EW tyWorker) =
        gets (IM.lookup wId) >>= \case
          Nothing -> mdo
            -- this instance is missing, we need to start it.
            let wrId = wId
            wrAsync <- liftIO $ async $ workerStart tyWorker sendMessage
            let sendMessage :: MessagePayload -> IO ()
                sendMessage mp = do
                  let tId = asyncThreadId wrAsync
                  t <- getCurrentTime
                  -- note that the reason that we are using thread id
                  -- instead of worker id is to prevent a "zombie" thread
                  -- from sending invalid data.
                  modifyMVar_ mQueue (pure . (Seq.|> (tId, (t, mp))))
            wrLastKnown <- liftIO getCurrentTime
            modify (IM.insert wId WorkerRep {..})
          Just WorkerRep {..} -> liftIO (poll wrAsync) >>= \case
            Nothing -> do
              curT <- liftIO getCurrentTime
              let lastContactDur = round $ diffUTCTime curT wrLastKnown
              when (lastContactDur > workerDeadline tyWorker) $ do
                liftIO $ do
                  putStrLn $
                    "Worker #" <> show wId
                    <> " doesn't contact within deadline, canceling it."
                  throwTo (asyncThreadId wrAsync) AsyncCancelled
                modify (IM.delete wId)
            Just r -> do
              liftIO $ do
                case r of
                  Left e -> do
                    putStrLn $ "Worker #" <> show wId <> " terminated with exception:"
                    putStrLn $ displayException e
                  Right () ->
                    putStrLn $ "Worker #" <> show wId <> " terminated normally."
                putStrLn $ "Worker #" <> show wId <> " will be restarted."
              -- thread is terminated, in either case, we should remove it from the record,
              -- so that we'll have them restarted in next loop.
              modify (IM.delete wId)

  V.imapM_ maintainWorker workersSpec
  liftIO $ threadDelay $ 200 * 1000

main :: IO ()
main = do
  (hOut, _) <- spawnDzen
  mQueue <- newMVar Seq.empty
  mainLoop hOut mQueue IM.empty

