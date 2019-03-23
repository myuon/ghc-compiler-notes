`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/GHC/Stack/Types.hs>`_

Note [Definition of CallStack]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CallStack is defined very early in base because it is
used by error and undefined. At this point in the dependency graph,
we do not have enough functionality to (conveniently) write a nice
pretty-printer for CallStack. The sensible place to define the
pretty-printer would be GHC.Stack, which is the main access point,
but unfortunately GHC.Stack imports GHC.Exception, which *needs*
the pretty-printer. So the CallStack type and functions are split
between three modules:

1. GHC.Stack.Types: defines the type and *simple* functions
2. GHC.Exception: defines the pretty-printer
3. GHC.Stack: exports everything and acts as the main access point

