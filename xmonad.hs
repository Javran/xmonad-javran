import System.Environment
import System.IO
import System.Process
import XMonad
import XMonad.Core
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run (spawnPipe)

import JavranXMonad.Config

main = do
    basePath <- getXMonadDir 
    let cmd = "/bin/bash " ++ initScript basePath
    hInit <- runCommand cmd
    -- TODO: next line kills xmonad itself...why?
    -- exitCode <- waitForProcess hInit
    -- spawn $ "/bin/bash " ++ initScript 

    -- TODO: use starkup hook instead of initScript
    dzenHandle <- spawnPipe dzenCommand
    _ <- spawnPipe $ conkyCommand basePath
    xmonad $ myConfig dzenHandle
