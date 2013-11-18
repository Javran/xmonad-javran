import System.IO
import System.Process
import XMonad
import XMonad.Hooks.DynamicLog

import JavranXMonad.Config

main = do
    spawn $ "/bin/bash " ++ initScript 
    c <- dzen myConfig
    xmonad c
