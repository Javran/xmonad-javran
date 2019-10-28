{-# LANGUAGE
    LambdaCase
  , OverloadedStrings
  #-}
module XMonad.Javran.SysInfoBar.TopProc
  ( TopProc
  ) where

import Control.Concurrent
import Control.Monad
import Data.Maybe
import Data.String
import System.Dzen
import System.IO
import System.Process

import XMonad.Javran.SysInfoBar.Types

data TopProc

renderTopProc :: Maybe String -> DString
renderTopProc = \case
    Nothing -> "------"
    Just xs | length xs <= 6 -> fromString $ take 6 (xs ++ repeat ' ')
    Just xs -> fromString $ take 4 xs ++ ".."

instance Worker TopProc where
  workerStart _ sendMessage = forever $ do
    let cp =
          (shell "ps --sort=-pcpu -eo comm | head -n 2")
            { std_in = NoStream
            , std_out = CreatePipe
            , std_err = Inherit
          }
    (_, Just hOut, _, _) <- createProcess cp
    (_:procNames) <- lines <$> hGetContents hOut
    sendMessage $ MPRendered $
      Just $ renderTopProc (listToMaybe procNames)
    threadDelay 500000

  workerDeadline _ = 5
