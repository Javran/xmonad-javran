{-# LANGUAGE OverloadedStrings #-}
module XMonad.Javran.SysInfoBar.Mail
  ( Mail
  ) where

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Data.Colour.SRGB
import Data.String
import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Connection
import Network.HaskellNet.IMAP.SSL
import System.Dzen
import System.Dzen.Internal (primStr)
import System.Environment
import System.IO
import Text.Printf

import XMonad.Javran.SysInfoBar.Types
import XMonad.Javran.Utils

data Mail

sleep :: IO ()
sleep = threadDelay (oneSec * 60 * 5)

oneSec :: Int
oneSec = 1000000


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

{-
  Note: investigated a bit about the runtime exception:

  - the problem seems to be in the response parsing of HaskellNet IMAP module.
  - look for pSearch function, which in turn calls searchCharset.
  - the query starts with "UID SEARCH".
 -}
-- TODO: logging facility
appendLog :: String -> IO ()
appendLog = appendLogTo "/tmp/mail.log"

renderMail :: Maybe Int -> DString
renderMail mCount =
    primStr (caPre <> content <> caPost) (Just (length content))
  where
    caPre, caPost :: String
    caPre = "^ca(1, xdg-open https://mail.google.com/)"
    caPost = "^ca()"
    content :: String
    content =
      case mCount of
        Nothing -> "----"
        Just count ->
          if count > 9999
            then ">=1k"
            else fromString (printf "%4d" count)

instance Worker Mail where
  workerStart _ sendMessage = run Nothing Nothing
    where
      retry prevRendered = do
        sendMessage (MPRendered prevRendered)
        sleep
        run Nothing prevRendered

      run :: Maybe IMAPConnection -> Maybe DString -> IO ()
      run mc@(Just conn) prevRendered = do
        mResult <- getUnreadMailCount conn
        case mResult of
          Just _ -> do
            let rendered = renderMail mResult
            sendMessage (MPRendered $ Just rendered)
            sleep
            -- apply a red color to previously rendered message,
            -- so that we have a visual indication that something is wrong.
            run mc (Just $ fg (sRGB24read "#FF0000") rendered)
          Nothing ->
            -- connection is down
            retry prevRendered
      run Nothing prevRendered = do
        -- try to prepare a connection if possible
        mc <- prepareConn
        case mc of
          Just _ ->
            run mc prevRendered
          Nothing ->
            retry prevRendered

  -- mail check runs every 5 minute,
  -- we'll consider the work dead if it fails to send a heartbeat
  -- within 5 min 30 sec.
  workerDeadline _ = 5 * 60 + 30
