{-# LANGUAGE TypeFamilies #-}
module XMonad.Javran.SysInfoBar.Mail
  ( MailWorker
  ) where

import System.IO
import System.Environment

import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Connection
import Network.HaskellNet.IMAP.SSL
import Control.Exception
import XMonad.Javran.SysInfoBar.Types
import Control.Concurrent
import Data.Typeable
import Data.Function
import qualified Data.Map.Strict as M

type AuthInfo = (String, String)

getAuthInfo :: IO AuthInfo
getAuthInfo = do
    homeDir <- getEnv "HOME"
    h <- openFile (homeDir ++ "/.xmonad_mail_check") ReadMode
    user <- hGetLine h
    pswd <- hGetLine h
    hClose h
    pure (user, pswd)

ioErrorHandler :: IOException -> IO (Maybe a)
ioErrorHandler _ = pure Nothing

prepareConn :: IO (Maybe IMAPConnection)
prepareConn = catch prep ioErrorHandler
  where
    prep = do
        c <- connectIMAPSSL "imap.gmail.com"
        (user, pswd) <- getAuthInfo
        authenticate c PLAIN user pswd
        select c  "Inbox"
        pure (Just c)

getUnreadMailCount :: IMAPConnection -> IO (Maybe Int)
getUnreadMailCount c = catch getCount ioErrorHandler
  where
    getCount = Just . length <$> search c [NOTs (FLAG Seen)]


runWorkerWith :: MVar BarState -> IO ()
runWorkerWith mv = run Nothing
  where
    run :: Maybe IMAPConnection -> IO ()
    run (Just conn) = fix $ \redo -> do
      mResult <- getUnreadMailCount conn
      case mResult of
        Just _ -> do
          let k = typeRep (Proxy :: Proxy MailWorker)  
          modifyMVar_ mv (pure . M.insert k (SomeWorkerState (St mResult)))
          sleep >> redo
        Nothing ->
          -- connection is down
          sleep >> run Nothing
    run Nothing = do
      -- try to prepare a connection if possible
      mc <- prepareConn
      case mc of
        Just _ -> run mc
        Nothing -> sleep >> run Nothing

    sleep :: IO ()
    sleep = threadDelay (oneSec * 60 * 10)
    oneSec =  1000000

data MailWorker

instance Worker MailWorker where
  data WState MailWorker = St (Maybe Int)
  type WStateRep MailWorker = Maybe Int
  runWorker _ = runWorkerWith
  getStateRep (St v) = v
