{-# LANGUAGE TypeOperators #-}
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath

import System.Info
import Data.List
import Control.Exception
import System.Environment as SE
import System.FilePath.Posix
import System.Directory as SD

type a :-> t = a

getXMonadDir :: IO FilePath
getXMonadDir = getAppUserDataDirectory "xmonad"

xmonadBinaryName :: String
xmonadBinaryName = "xmonad-"++arch++"-"++os

-- get and verify project directory, so we can make the script independent
-- of the current directory of the script
getProjectDirectory :: IO String
getProjectDirectory = do
    putStrLn "Getting and verifying project directory."
    putStrLn "Please make sure $XMONAD_HOME points to the correct directory"
    putStrLn "and file \"xmonad-javran.cabal\" is in the directory"
    prjDir <- SE.getEnv "XMONAD_HOME"
    let cabalFile = prjDir </> "xmonad-javran.cabal"
    True <- SD.doesFileExist cabalFile
    return prjDir

main :: IO ()
main = do
  projectDir <- getProjectDirectory
  putStrLn $ "Setting current directory to: " ++ projectDir
  SD.setCurrentDirectory projectDir

  shakeArgs shakeOptions{shakeFiles="_build/"} $ do
    let binaries = map ("_build" </>)
                 . words
                 $ "xmonad-javran MailChecker StreamConverter"
        syncEtcs = "syncEtcs"
        syncBins = "syncBins"
        diffEtcs = "diffEtcs"
        xmBin    = "_build" </> xmonadBinaryName
        cmdCb    :: CmdArguments args => args :-> Action r
        cmdCb    = cmd Shell (Cwd "cabal-zone")
        info     = putNormal . ("## ShakeBuild: " ++)
        collectSrcs = getDirectoryFiles "" [ "src//*.hs"
                                           , "xmonad-javran.cabal"
                                           ]

    want ["all"]

    binaries &%> \ _ -> do
        let binDir = projectDir </> "_build"
        srcFiles <- collectSrcs
        need srcFiles
        info "Building and copying binaries"
        -- make it think it's copying binaries to a PATH location
        fakePath <- addPath [binDir] []
        let shellCmd = "stack --local-bin-path " ++ binDir ++ " install"
        cmd Shell fakePath shellCmd  :: Action ()

    xmBin %> \ _ -> do
        need binaries
        cmd Shell (Cwd "_build") "cp xmonad-javran" xmonadBinaryName

    phony "all" $ do
        need [syncBins, syncEtcs]
        info "Printing current time"
        cmd "date"

    phony syncEtcs $ do
        projectDir <- liftIO getCurrentDirectory
        xmDir <- liftIO getXMonadDir
        info "Synchronizing etc files"
        cmd "rsync -uarc" (projectDir </> "etc/") xmDir :: Action ()
        need [diffEtcs]

    phony syncBins $ do
        projectDir <- liftIO getCurrentDirectory
        xmDir <- liftIO getXMonadDir
        info "Synchronizing binary files"
        need (xmBin:binaries)
        cmd "rsync -uarc --exclude .database" (projectDir </> "_build/") xmDir

    phony diffEtcs $ do
        xmDir <- liftIO getXMonadDir
        info "Comparing etc files' differences"
        -- http://stackoverflow.com/q/11325123/315302
        (Exit _, Stdout out) <- cmd (Env [("LC_ALL","C")]) "diff -r etc/" xmDir
        liftIO . mapM_ putStrLn
               . filter (not . isPrefixOf "Only in")
               . lines $ out

    phony "clean" $ do
        info "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]
        info "Cleaning cabal-zone"
        cmdCb "cabal clean"
