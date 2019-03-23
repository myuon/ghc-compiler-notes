`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/Control/Monad.hs>`_

Note [Worker/wrapper transform on replicateM/replicateM_]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The implementations of replicateM and replicateM_ both leverage the
worker/wrapper transform. The simpler implementation of replicateM_, as an
example, would be:

    replicateM_ 0 _ = pure ()
    replicateM_ n f = f *> replicateM_ (n - 1) f

However, the self-recursive nature of this implementation inhibits inlining,
which means we never get to specialise to the action (`f` in the code above).
By contrast, the implementation below with a local loop makes it possible to
inline the entire definition (as happens for foldr, for example) thereby
specialising for the particular action.

For further information, see this issue comment, which includes side-by-side
Core: https://gitlab.haskell.org/ghc/ghc/issues/11795#note_118976

