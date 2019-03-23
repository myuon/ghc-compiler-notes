`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/cmm/Bitmap.hs>`_

Note [Strictness when building Bitmaps]
========================================

One of the places where @Bitmap@ is used is in in building Static Reference
Tables (SRTs) (in @CmmBuildInfoTables.procpointSRT@). In #7450 it was noticed
that some test cases (particularly those whose C-- have large numbers of CAFs)
produced large quantities of allocations from this function.

The source traced back to 'intsToBitmap', which was lazily subtracting the word
size from the elements of the tail of the @slots@ list and recursively invoking
itself with the result. This resulted in large numbers of subtraction thunks
being built up. Here we take care to avoid passing new thunks to the recursive
call. Instead we pass the unmodified tail along with an explicit position
accumulator, which get subtracted in the fold when we compute the Word.


