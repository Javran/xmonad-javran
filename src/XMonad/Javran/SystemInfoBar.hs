module XMonad.Javran.SystemInfoBar where

{-
  WIP.

  this module aims at eliminating the need for
  current conky ==> StreamConverter ==> dzen pipeline,
  simplifying it into a single SystemInfoBar,
  in which it manages a separated instance of dzen.
-}

{-
  TODO

  system info we are looking at:

  - [ ] CPU usage for each individual ones

    + http://e2e.ti.com/support/legacy_forums/embedded/linux/f/354/t/221192

  - [ ] CPU freq
  - [ ] memory usage
  - [ ] network Rx & Tx
  - [ ] mail checker
  - [ ] mpd state
  - [ ] whether battery is charging
  - [ ] battery remaining
  - [ ] date
  - [ ] time

  + stage 1 is to grab these info in a constant interval (say 1 sec)
  + stage 2 is to have a process working on this, and impl another component
    to render things in dzen-format



 -}

main :: IO ()
main = pure ()
