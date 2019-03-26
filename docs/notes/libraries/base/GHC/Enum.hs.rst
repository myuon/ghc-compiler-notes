`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/GHC/Enum.hs>`_

libraries/base/GHC/Enum.hs
==========================


Note [How the Enum rules work]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/GHC/Enum.hs#L500>`__

* Phase 2: eftInt ---> build . eftIntFB
* Phase 1: inline build; eftIntFB (:) --> eftInt
* Phase 0: optionally inline eftInt

