`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/utils/UniqSet.hs>`_

compiler/utils/UniqSet.hs
=========================


Note [UniqSet invariant]
~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/utils/UniqSet.hs#L58>`__

UniqSet has the following invariant:
  The keys in the map are the uniques of the values
It means that to implement mapUniqSet you have to update
both the keys and the values.

