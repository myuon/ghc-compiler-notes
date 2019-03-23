`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/utils/Encoding.hs>`_

Note [Base 62 encoding 128-bit integers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Instead of base-62 encoding a single 128-bit integer
(ceil(21.49) characters), we'll base-62 a pair of 64-bit integers
(2 * ceil(10.75) characters).  Luckily for us, it's the same number of
characters!
------------------------------------------------------------------------
 Base 62
 The base-62 code is based off of 'locators'
 ((c) Operational Dynamics Consulting, BSD3 licensed)

