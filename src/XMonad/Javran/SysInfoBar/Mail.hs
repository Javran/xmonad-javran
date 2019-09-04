{-# LANGUAGE TypeFamilies #-}
module XMonad.Javran.SysInfoBar.Mail
  ( Mail
  ) where

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Data.Function
import Data.Typeable
import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Connection
import Network.HaskellNet.IMAP.SSL
import System.Environment
import System.IO

import qualified Data.Map.Strict as M

import XMonad.Javran.SysInfoBar.Types
import XMonad.Javran.Utils

type AuthInfo = (String, String)

getAuthInfo :: IO AuthInfo
getAuthInfo = do
    homeDir <- getEnv "HOME"
    h <- openFile (homeDir ++ "/.xmonad_mail_check") ReadMode
    user <- hGetLine h
    pswd <- hGetLine h
    hClose h
    pure (user, pswd)

{-
  capture all exceptions happened in the library call,
  this is a shotgun approach, but for our task that is required to
  run all the time, this is the safest way.
 -}
errHandler :: SomeException -> IO (Maybe a)
errHandler e = do
  appendLog $ "exception caught: " ++ displayException e
  pure Nothing

prepareConn :: IO (Maybe IMAPConnection)
prepareConn = catch prep errHandler
  where
    prep = do
        c <- connectIMAPSSL "imap.gmail.com"
        (user, pswd) <- getAuthInfo
        authenticate c PLAIN user pswd
        select c  "Inbox"
        pure (Just c)

getUnreadMailCount :: IMAPConnection -> IO (Maybe Int)
getUnreadMailCount c = catch getCount errHandler
  where
    getCount = Just . length <$!!> search c [NOTs (FLAG Seen)]

appendLog :: String -> IO ()
appendLog = appendLogTo "/tmp/mail.log"

runWorkerWith :: MVar BarState -> IO ()
runWorkerWith mv = run Nothing
  where
    run :: Maybe IMAPConnection -> IO ()
    run (Just conn) = fix $ \redo -> do
      mResult <- getUnreadMailCount conn
      case mResult of
        Just _ -> do
          let k = typeRep (Proxy :: Proxy Mail)
          modifyMVar_ mv (pure . M.insert k (SomeWorkerState (St mResult)))
          sleep >> redo
        Nothing ->
          -- connection is down
          sleep >> run Nothing
    run Nothing = do
      -- try to prepare a connection if possible
      mc <- prepareConn
      case mc of
        Just _ ->
          run mc
        Nothing ->
          sleep >>
          run Nothing

    sleep :: IO ()
    sleep = threadDelay (oneSec * 60 * 5)
    oneSec =  1000000

data Mail

instance Worker Mail where
  data WState Mail = St (Maybe Int)
  type WStateRep Mail = Maybe Int
  runWorker _ = runWorkerWith
  getStateRep (St v) = v
