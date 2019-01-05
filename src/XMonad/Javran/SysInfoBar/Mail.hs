{-# LANGUAGE TypeFamilies #-}
module XMonad.Javran.SysInfoBar.Mail
  ( Mail
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
import Data.Time

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
  TODO: changing to capture all errors in hope that we can understand what's broken

  NOTE: exception caught:

  Data.ByteString.last: empty ByteString
  CallStack (from HasCallStack):
    error, called at libraries/bytestring/Data/ByteString.hs:1877:23 in bytestring-0.10.8.2:Data.ByteString

 -}
ioErrorHandler :: SomeException -> IO (Maybe a)
ioErrorHandler e = do
  appendLog $ "exception caught: " ++ displayException e
  pure Nothing

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

appendLog :: String -> IO ()
appendLog msg = do
  t <- getZonedTime
  let dateStr = formatTime defaultTimeLocale "%_Y-%m-%d" t
      timeStr = formatTime defaultTimeLocale "%T" t
      header = "[" <> dateStr <> " " <> timeStr <> "]"
  appendFile "/tmp/mail.log" (header <> " " <> msg <> "\n")
  
runWorkerWith :: MVar BarState -> IO ()
runWorkerWith mv =
    appendLog "start working" >>
    run Nothing
  where
    run :: Maybe IMAPConnection -> IO ()
    run (Just conn) = fix $ \redo -> do
      mResult <- getUnreadMailCount conn
      case mResult of
        Just _ -> do
          appendLog "mail checked successfully"
          let k = typeRep (Proxy :: Proxy Mail)
          modifyMVar_ mv (pure . M.insert k (SomeWorkerState (St mResult)))
          sleep >> redo
        Nothing ->
          appendLog "connection is down" >>
          -- connection is down
          sleep >> run Nothing
    run Nothing = do
      appendLog "prepare connection"
      -- try to prepare a connection if possible
      mc <- prepareConn
      case mc of
        Just _ ->
          appendLog "connection established" >>
          run mc
        Nothing ->
          appendLog "cannot establish connection" >>
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
