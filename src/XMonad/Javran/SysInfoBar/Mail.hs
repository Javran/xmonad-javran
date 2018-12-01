{-# LANGUAGE ScopedTypeVariables #-}
module XMonad.Javran.SysInfoBar.Mail where

import System.IO
import System.Environment

import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Connection
import Network.HaskellNet.IMAP.SSL
import Control.Exception

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
