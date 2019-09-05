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

{-
  Design revisit:

  - there's no need of storing states in a shared dict.
    we can just let workers pass rendered messages (in the form of thunk),
    together with a timestamp. so main thread knows that a thread
    might be dead for random reasons and try to kill and restart it.

  - messages goes into a queue in main thread, which then reads and handles the actual updating
    (optimization: if multiple messages are passed from same thread, we can only take the latest one)

  - for the health check idea above to work, defined a deadline for each worker.

  - threads should really be identified with thread ids - using type as identifier
    limits us to use one thread for each type, which is not necessary.

 -}
