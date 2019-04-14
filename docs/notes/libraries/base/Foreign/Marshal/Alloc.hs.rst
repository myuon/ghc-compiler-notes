`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/Foreign/Marshal/Alloc.hs>`_

libraries/base/Foreign/Marshal/Alloc.hs
=======================================


Note [NOINLINE for touch#]
~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/Foreign/Marshal/Alloc.hs#L119>`__

Both allocaBytes and allocaBytesAligned use the touch#, which is notoriously
fragile in the presence of simplification (see #14346). In particular, the
simplifier may drop the continuation containing the touch# if it can prove
that the action passed to allocaBytes will not return. The hack introduced to
fix this for 8.2.2 is to mark allocaBytes as NOINLINE, ensuring that the
simplifier can't see the divergence.

These can be removed once #14375 is fixed, which suggests that we instead do
away with touch# in favor of a primitive that will capture the scoping left
implicit in the case of touch#.

