`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/utils/Encoding.hs>`_

compiler/utils/Encoding.hs
==========================


Note [Base 62 encoding 128-bit integers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/utils/Encoding.hs#L416>`__

Instead of base-62 encoding a single 128-bit integer
(ceil(21.49) characters), we'll base-62 a pair of 64-bit integers
(2 * ceil(10.75) characters).  Luckily for us, it's the same number of
characters!

