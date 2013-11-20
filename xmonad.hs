import System.IO
import System.Process
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run (spawnPipe)

import JavranXMonad.Config

import System.IO
import System.Environment
import System.Process

main = do
    let cmd = "/bin/bash " ++ initScript
    hInit <- runCommand cmd
    -- TODO: next line kills xmonad itself...why?
    -- exitCode <- waitForProcess hInit
    -- spawn $ "/bin/bash " ++ initScript 
    dzenHandle <- spawnPipe dzenCommand
    _ <- spawnPipe conkyCommand 
    xmonad $ myConfig dzenHandle
