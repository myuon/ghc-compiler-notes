`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CoreFVs.hs>`_

Note [The FVAnn invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Invariant: a FVAnn, say S, is closed:
  That is: if v is in S,
           then freevars( v's type/kind ) is also in S

