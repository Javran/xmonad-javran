- since xmonad now allows custom build, entryhelper can be removed

- TODO: remove uses of `<Hyper>` key

  + the idea is that having `<Super>` and `<Hyper>` doesn't play well
    with VNC and perhaps other remote desktop applications
  + for now `<Hyper>` is for xmonad-related controls and `<Super>` for
    launching programs
  + We'll like to have `<Super>` to do xmonad-related things, and
    `<Super>+something <some other key>` for launching programs,
    which can be done using emacs-style key bindings from xmonad-contrib
