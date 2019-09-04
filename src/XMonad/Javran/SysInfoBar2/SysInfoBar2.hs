module XMonad.Javran.SysInfoBar2.SysInfoBar2 where

{-
  A playground for trying out idea for improved design of SysInfoBar.

  - now I feel the current use case is not a very good use of existential types,
    as we still have many boilerplate to write and in addition,
    we don't get much since all plugins' interfaces are the same.
    (also we do need to have render logic for each of them in one place -
    there is no actual point of the current isolation provided by existential type.

 - switch to async tasks. I feel that way we can get a bit more control.

 - we'll also need a common data type for processes to hold their state or
   send response back.

 -}
