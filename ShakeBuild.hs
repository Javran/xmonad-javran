{-# LANGUAGE TypeOperators, TypeApplications #-}
import Development.Shake
import Development.Shake.FilePath

import Control.Exception
import Data.List
import System.Directory as SD
import System.Environment as SE
import System.Exit
import System.IO
import System.IO.Error
import System.Info

getXMonadDir :: IO FilePath
getXMonadDir = getAppUserDataDirectory "xmonad"

xmonadBinaryName :: String
xmonadBinaryName = "xmonad-" <> arch <> "-" <> os

-- get and verify project directory, so we can make the script independent
-- of the current directory of the script
getProjectDirectory :: IO String
getProjectDirectory = do
    putStrLn "Getting and verifying project directory ..."
    let doChecks = do
          prjDir <- SE.getEnv "XMONAD_HOME"
          let cabalFile = prjDir </> "xmonad-javran.cabal"
          b <- SD.doesFileExist cabalFile
          if b
            then pure prjDir
            else do
              hPutStrLn stderr "Please make sure $XMONAD_HOME points to the correct directory,"
              hPutStrLn stderr "  and file \"xmonad-javran.cabal\" is in the directory."
              throw $ mkIOError doesNotExistErrorType "?" Nothing (Just cabalFile)
    catch @IOException doChecks $ \e -> do
      hPutStrLn stderr $ "error: " <> displayException e
      exitFailure

main :: IO ()
main = do
  projectDir <- getProjectDirectory
  putStrLn $ "Setting current directory to: " ++ projectDir
  SD.setCurrentDirectory projectDir

  shakeArgs shakeOptions{shakeFiles="_build/"} $ do
    let binaries = map ("_build" </>)
                 . words
                 $ "xmonad-javran SysInfoBar"
        syncEtcs = "syncEtcs"
        syncBins = "syncBins"
        diffEtcs = "diffEtcs"
        xmBin    = "_build" </> xmonadBinaryName
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
        xmDir <- liftIO getXMonadDir
        info "Synchronizing etc files"
        cmd "rsync -uarc" (projectDir </> "etc/") xmDir :: Action ()
        need [diffEtcs]

    phony syncBins $ do
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
        cmd "stack clean"
