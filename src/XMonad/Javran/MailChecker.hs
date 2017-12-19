module XMonad.Javran.MailChecker
  ( main
  ) where

import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.SSL
import System.IO
import System.Environment

doMailCheck :: String -> String -> IO ()
doMailCheck user pswd = do
    c <- connectIMAPSSL "imap.gmail.com"
    authenticate c PLAIN user pswd
    select c  "Inbox"
    uids <- search c [NOTs (FLAG Seen)]
    h <- openFile "/tmp/xmonad_mail_check" WriteMode
    hPrint h (length uids)
    hClose h
    logout c

main :: IO ()
main = do
    homeDir <- getEnv "HOME"
    h <- openFile (homeDir ++ "/.xmonad_mail_check") ReadMode
    user <- hGetLine h
    pswd <- hGetLine h

    doMailCheck user pswd
    hClose h
