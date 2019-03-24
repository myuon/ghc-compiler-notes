`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/GHC/Enum.hs>`_

====================
libraries/base/GHC/Enum.hs.rst
====================

Note [How the Enum rules work]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Phase 2: eftInt ---> build . eftIntFB
* Phase 1: inline build; eftIntFB (:) --> eftInt
* Phase 0: optionally inline eftInt

