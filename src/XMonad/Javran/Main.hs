module XMonad.Javran.Main where

import System.Process
import XMonad
import XMonad.Util.Run (spawnPipe)
import System.Exit
import XMonad.Javran.Config

import qualified XMonad.Util.EntryHelper as EH

main :: IO ()
main = EH.withCustomHelper mhConf
  where
    mhConf = EH.defaultConfig  {
          EH.run = do
                basePath <- getXMonadDir
                let cmd = "/bin/bash " ++ initScript basePath
                _ <- runCommand cmd
                -- TODO: next line kills xmonad itself...why?
                -- exitCode <- waitForProcess hInit
                -- spawn $ "/bin/bash " ++ initScript
                -- TODO: use startup hook instead of initScript
                dzenHandle <- spawnPipe dzenCommand
                _ <- spawnPipe $ conkyCommand basePath
                xmonad (myConfig dzenHandle)
        , EH.compile = \force -> EH.withLock ExitSuccess $ do
              let cmd = if force
                        then "cd ${XMONAD_HOME} && ./build.sh clean && ./build.sh all"
                        else "cd ${XMONAD_HOME} && ./build.sh all"
              EH.compileUsingShell cmd
        }

