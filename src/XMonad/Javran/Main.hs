{-# LANGUAGE TypeApplications #-}
module XMonad.Javran.Main
  ( main
  ) where

import System.Process
import System.Exit
import System.FilePath.Posix

import XMonad
import qualified XMonad.Util.EntryHelper as EH
import XMonad.Util.Run (spawnPipe)

import XMonad.Javran.Config

dzenCommand :: String
dzenCommand = unwords
    [ "dzen2"
    , "-x" , sI 0
    , "-w" , sI 900
    , "-ta", "l"
    , "-h" , sI 24
    , "-fg", "\"#22EE11\""
    , "-bg", "\"#202020\""
    , "-fn", "\"WenQuanYi MicroHei Mono:pixelsize=15:antialias=true\""
    , "-e", "\"button2=;\""
    --, "-l", "5"
    ]
  where
    sI = show @Int

main :: IO ()
main = EH.withCustomHelper mhConf
  where
    mhConf = EH.defaultConfig  {
          EH.run = do
                basePath <- getXMonadDir
                let cmd = "/bin/bash " ++ (basePath </> "xmonad-init.sh")
                _ <- runCommand cmd
                -- TODO: next line kills xmonad itself...why?
                -- exitCode <- waitForProcess hInit
                -- spawn $ "/bin/bash " ++ initScript
                -- TODO: use startup hook instead of initScript
                -- TODO: this part seems to be executed twice, find out what happened.
                dzenHandle <- spawnPipe dzenCommand
                xmonad (myConfig dzenHandle)
        , EH.compile = \force -> EH.withLock ExitSuccess $ do
              let cmd = if force
                        then "cd ${XMONAD_HOME} && ./build.sh clean && ./build.sh all"
                        else "cd ${XMONAD_HOME} && ./build.sh all"
              EH.compileUsingShell cmd
        }

