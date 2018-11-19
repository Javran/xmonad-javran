module XMonad.Javran.Main where

import System.Process
import System.Exit
import System.FilePath.Posix

import XMonad
import qualified XMonad.Util.EntryHelper as EH
import XMonad.Util.Run (spawnPipe)

import XMonad.Javran.Config

conkyConf              :: FilePath -> FilePath
pathStreamConvert      :: FilePath -> FilePath
pathStreamConvertConf  :: FilePath -> FilePath
initScript             :: FilePath -> FilePath
pathStreamConvert      = (</> "StreamConverter")
pathStreamConvertConf  = (</> "stream_convert.txt")
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

conkyCommand :: FilePath -> String
conkyCommand xmPath = unwords
    [ "pkill -9 conky"
    , ";"
    , "conky"
    , "-c", conkyConf xmPath
    , "|"
    , pathStreamConvert xmPath
    , pathStreamConvertConf xmPath
    , "|"
    , "dzen2"
    , "-w", showI 810
    , "-x", showI 900
    , "-h", showI 24
    , "-fn", "\"DejaVu Sans Mono:pixelsize=15:antialias=true\""
    , "-bg", "\"#505050\""
    , "-e", "\"button2=;\""
    -- , "-l", "4"
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
                dzenHandle <- spawnPipe dzenCommand
                _ <- spawnPipe $ conkyCommand basePath
                xmonad (myConfig dzenHandle)
        , EH.compile = \force -> EH.withLock ExitSuccess $ do
              let cmd = if force
                        then "cd ${XMONAD_HOME} && ./build.sh clean && ./build.sh all"
                        else "cd ${XMONAD_HOME} && ./build.sh all"
              EH.compileUsingShell cmd
        }

