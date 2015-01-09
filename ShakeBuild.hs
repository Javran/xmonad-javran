{-# LANGUAGE TypeOperators #-}
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath

import System.Directory
import System.Info
import Data.List

type a :-> t = a

getXMonadDir :: IO FilePath
getXMonadDir = getAppUserDataDirectory "xmonad"

xmonadBinaryName :: String
xmonadBinaryName = "xmonad-"++arch++"-"++os

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
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
        collectSrcs = getDirectoryFiles "" [ "cabal-zone/src//*.hs"
                                           , "cabal-zone/xmonad-javran.cabal"
                                           ]

    want ["all"]

    binaries &%> \ _ -> do
        projectDir <- liftIO getCurrentDirectory
        let binDir = projectDir </> "_build"
        srcFiles <- collectSrcs
        need srcFiles
        info "Building binaries"
        cmdCb "cabal configure" ("--bindir=" ++ binDir) :: Action ()
        cmdCb "cabal build" :: Action ()
        info "Copying binaries"
        -- make it think it's copying binaries to a PATH location
        fakePath <- addPath [binDir] []
        cmdCb fakePath "cabal copy" :: Action ()

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
