`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/llvmGen/Llvm/Types.hs>`_

Note [LLVM Float Types]
~~~~~~~~~~~~~~~~~~~~~~~
We use 'ppDouble' for both printing Float and Double floating point types. This is
as LLVM expects all floating point constants (single & double) to be in IEEE
754 Double precision format. However, for single precision numbers (Float)
they should be *representable* in IEEE 754 Single precision format. So the
easiest way to do this is to narrow and widen again.
(i.e., Double -> Float -> Double). We must be careful doing this that GHC
doesn't optimize that away.


Note [narrowFp & widenFp]
~~~~~~~~~~~~~~~~~~~~~~~~~
NOTE: we use float2Double & co directly as GHC likes to optimize away
successive calls of 'realToFrac', defeating the narrowing. (Bug #7600).
'realToFrac' has inconsistent behaviour with optimisation as well that can
also cause issues, these methods don't.

