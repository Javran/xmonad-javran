module XMonad.Javran.Main where

import System.Process
import System.Exit
import System.FilePath.Posix

import XMonad
import qualified XMonad.Util.EntryHelper as EH
import XMonad.Util.Run (spawnPipe)

import XMonad.Javran.Config

conkyConf              :: FilePath -> FilePath
pathSysInfoBar         :: FilePath -> FilePath
initScript             :: FilePath -> FilePath
pathSysInfoBar         = (</> "SysInfoBar")
conkyConf              = (</> "conky-json.lua")
initScript             = (</> "xmonad-init.sh")

showI :: Int -> String
showI = show

dzenCommand :: String
dzenCommand = unwords
    [ "dzen2"
    , "-x" , showI 0
    , "-w" , showI 900
    , "-ta", "l"
    , "-h" , showI 24
    , "-fg", "\"#22EE11\""
    , "-bg", "\"#202020\""
    , "-fn", "\"WenQuanYi MicroHei Mono:pixelsize=15:antialias=true\""
    , "-e", "\"button2=;\""
    --, "-l", "5"
    ]

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
                -- TODO: this part seems to be executed twice, find out what happened.
                dzenHandle <- spawnPipe dzenCommand
                xmonad (myConfig dzenHandle)
        , EH.compile = \force -> EH.withLock ExitSuccess $ do
              let cmd = if force
                        then "cd ${XMONAD_HOME} && ./build.sh clean && ./build.sh all"
                        else "cd ${XMONAD_HOME} && ./build.sh all"
              EH.compileUsingShell cmd
        }

