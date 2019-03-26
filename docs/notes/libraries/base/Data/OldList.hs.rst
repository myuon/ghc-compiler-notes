`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/Data/OldList.hs>`_

libraries/base/Data/OldList.hs
==============================


Note [Inline @unzipN@ functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/Data/OldList.hs#L934>`__

The inline principle for @unzip{4,5,6,7}@ is the same as 'unzip'/'unzip3' in
"GHC.List".
The 'unzip'/'unzip3' functions are inlined so that the `foldr` with which they
are defined has an opportunity to fuse.

As such, since there are not any differences between 2/3-ary 'unzip' and its
n-ary counterparts below aside from the number of arguments, the `INLINE`
pragma should be replicated in the @unzipN@ functions below as well.

