module Main where

import System.Process
import XMonad
import XMonad.Util.Run (spawnPipe)

import JavranXMonad.Config

main :: IO ()
main = do
    basePath <- getXMonadDir
    let cmd = "/bin/bash " ++ initScript basePath
    _ <- runCommand cmd
    -- TODO: next line kills xmonad itself...why?
    -- exitCode <- waitForProcess hInit
    -- spawn $ "/bin/bash " ++ initScript

    -- TODO: use starkup hook instead of initScript
    dzenHandle <- spawnPipe dzenCommand
    _ <- spawnPipe $ conkyCommand basePath
    xmonad $ myConfig dzenHandle
