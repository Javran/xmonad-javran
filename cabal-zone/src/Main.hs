module Main where

import System.Process
import XMonad
import XMonad.Util.Run (spawnPipe)
import System.Environment

import JavranXMonad.Config

import qualified XMonad.Util.MainHelper as MH

main :: IO ()
main = MH.withCustomHelper mhConf
  where
    mhConf = MH.defaultConfig  {
          MH.execute = do
                basePath <- getXMonadDir
                let cmd = "/bin/bash " ++ initScript basePath
                _ <- runCommand cmd
                -- TODO: next line kills xmonad itself...why?
                -- exitCode <- waitForProcess hInit
                -- spawn $ "/bin/bash " ++ initScript
                -- TODO: use starkup hook instead of initScript
                dzenHandle <- spawnPipe dzenCommand
                _ <- spawnPipe $ conkyCommand basePath
                xmonad (myConfig dzenHandle)
        , MH.upToDateCheck = return False
        , MH.recompileCommand = do
            xmonadHome <- getEnv "XMONAD_HOME"
            return ( "./build.sh", ["all"], Just xmonadHome)
        }
