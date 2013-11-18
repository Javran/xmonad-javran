import System.IO
import System.Process
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.Replace

import JavranXMonad.Config

main = xmonad =<< dzen myConfig
