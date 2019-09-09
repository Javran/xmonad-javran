module XMonad.Javran.SysInfoBar2.Mail
  ( Mail
  ) where

import Control.Concurrent
import Data.Colour.SRGB
import Network.HaskellNet.IMAP.Connection
import System.Dzen
import XMonad.Javran.SysInfoBar.DzenRender (renderMail)
import XMonad.Javran.SysInfoBar.Mail (getUnreadMailCount, prepareConn)
import XMonad.Javran.SysInfoBar2.Types

data Mail

sleep :: IO ()
sleep = threadDelay (oneSec * 60 * 5)

oneSec :: Int
oneSec = 1000000

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
